/*
 * GHMITER (hypercube) secondary suppression for tau-ARGUS.
 *
 * C port of the GHMITER 4 program (Gießing/Repsilber), originally
 * Copyright IT Nordrhein Westfalen, published under EUPL 1.1 by
 * sdcTools (https://github.com/sdcTools/hypercube). The original
 * Fortran is kept in reference/fortran as the porting reference and
 * test oracle.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the European Union Public Licence
 * (EUPL) version 1.1, as published by the European Commission.
 *
 * You can find the text of the EUPL v1.1 on
 * https://joinup.ec.europa.eu/software/page/eupl/licence-eupl
 *
 * This software is distributed on an "AS IS" basis without
 * warranties or conditions of any kind, either express or implied.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <math.h>
#include <stdint.h>

#include "ghm.h"

/*
 * Collect every single-quoted token in a line into a growing array.
 * Returns the number of tokens stored, or -1 on malformed input or
 * allocation failure (releasing all tokens already collected). Each token is a
 * malloc'd NUL-terminated string without the surrounding quotes.
 */
static int collect_quoted(const char *line, char ***out, int *cap)
{
    int i = 0;
    const char *p = line;
    while (*p) {
        while (isspace((unsigned char)*p)) p++;
        if (!*p) break;
        if (*p != '\'') goto fail;
        const char *start = p + 1;
        const char *end = strchr(start, '\'');
        if (end == NULL)
            goto fail;
        if (i == 12) goto fail; /* at most six keys and six labels */
        if (i == *cap) {
            int ncap = *cap ? *cap * 2 : 8;
            char **tmp = realloc(*out, (size_t)ncap * sizeof(**out));
            if (!tmp)
                goto fail;
            *out = tmp;
            *cap = ncap;
        }
        size_t len = (size_t)(end - start);
        char *tok = malloc(len + 1);
        if (!tok)
            goto fail;
        memcpy(tok, start, len);
        tok[len] = '\0';
        (*out)[i++] = tok;
        p = end + 1;
        if (*p && !isspace((unsigned char)*p)) goto fail;
    }
    return i;
fail:
    for (int j = 0; j < i; j++) free((*out)[j]);
    free(*out);
    *out = NULL;
    *cap = 0;
    return -1;
}

static void free_quoted(char **quoted, int n)
{
    if (!quoted)
        return;
    for (int i = 0; i < n; i++)
        free(quoted[i]);
    free(quoted);
}

/* Modified 2026-11-12: checked conversions avoid scanf integer overflow
 * and reject nonfinite numbers before any records become solver input. */
static int read_integer(const char **cursor, long *value)
{
    char *end;
    const char *p = *cursor;
    while (isspace((unsigned char)*p)) p++;
    errno = 0;
    *value = strtol(p, &end, 10);
    if (p == end || errno == ERANGE || (*end && !isspace((unsigned char)*end)))
        return -1;
    *cursor = end;
    return 0;
}

static int read_int(const char **cursor, int *value)
{
    long n;
    if (read_integer(cursor, &n) || n < INT_MIN || n > INT_MAX) return -1;
    *value = (int)n;
    return 0;
}

static int read_real(const char **cursor, double *value)
{
    char *end;
    const char *p = *cursor;
    while (isspace((unsigned char)*p)) p++;
    errno = 0;
    *value = strtod(p, &end);
    if (p == end || errno == ERANGE || !isfinite(*value) ||
        (*end && !isspace((unsigned char)*end))) return -1;
    *cursor = end;
    return 0;
}

static const char *parse_prefix(const char *line, ghm_cell *c)
{
    const char *p = line;
    if (read_int(&p, &c->wertart) || read_integer(&p, &c->freq) ||
        read_real(&p, &c->resp) || read_int(&p, &c->gewi) ||
        read_int(&p, &c->hoew) || read_real(&p, &c->fehl0) ||
        read_real(&p, &c->fehl1)) return NULL;
    while (isspace((unsigned char)*p)) p++;
    return *p == '\'' ? p : NULL;
}

int ghm_parse_cells(const char *path, ghm_cell **out, long *n,
                    char *errbuf, size_t errlen)
{
    FILE *fd;
    long count = 0, cap = 0;
    ghm_cell *cells = NULL;
    char line[8192];

    if (!path || !out || !n) {
        if (errbuf && errlen)
            snprintf(errbuf, errlen, "null argument");
        return -1;
    }
    *out = NULL;
    *n = 0;

    fd = fopen(path, "r");
    if (!fd) {
        if (errbuf && errlen)
            snprintf(errbuf, errlen, "cannot open cells file: %s", path);
        return -1;
    }

    while (fgets(line, sizeof(line), fd)) {
        ghm_cell parsed;
        const char *quoted_start;
        char **quoted = NULL;
        int qcap = 0, nq;
        long ndim;
        ghm_cell *c;

        if (!strchr(line, '\n') && !feof(fd)) {
            if (errbuf && errlen) snprintf(errbuf, errlen, "cell record too long");
            fclose(fd);
            ghm_free_cells(cells, count);
            return -1;
        }

        /* Skip blank lines. */
        {
            const char *k = line;
            while (*k == ' ' || *k == '\t')
                k++;
            if (*k == '\n' || *k == '\r' || *k == '\0')
                continue;
        }

        memset(&parsed, 0, sizeof parsed);
        quoted_start = parse_prefix(line, &parsed);
        if (!quoted_start) {
            if (errbuf && errlen)
                snprintf(errbuf, errlen, "malformed cell record: %s", line);
            fclose(fd);
            ghm_free_cells(cells, count);
            return -1;
        }

        nq = collect_quoted(quoted_start, &quoted, &qcap);
        if (nq < 2 || nq > 12 || (nq % 2) != 0) {
            if (errbuf && errlen)
                snprintf(errbuf, errlen,
                         "expected 1..6 key/label pairs, got %d quoted tokens",
                         nq);
            if (nq >= 0)
                free_quoted(quoted, nq);
            fclose(fd);
            ghm_free_cells(cells, count);
            return -1;
        }
        ndim = nq / 2;

        if (count == cap) {
            if (cap > LONG_MAX / 2 || (size_t)cap > SIZE_MAX / (2 * sizeof(*cells))) {
                if (errbuf && errlen) snprintf(errbuf, errlen, "too many cell records");
                free_quoted(quoted, nq);
                fclose(fd);
                ghm_free_cells(cells, count);
                return -1;
            }
            long ncap = cap ? cap * 2 : 16;
            ghm_cell *tmp = realloc(cells, (size_t)ncap * sizeof(*cells));
            if (!tmp) {
                if (errbuf && errlen)
                    snprintf(errbuf, errlen, "out of memory");
                free_quoted(quoted, nq);
                fclose(fd);
                ghm_free_cells(cells, count);
                return -1;
            }
            cells = tmp;
            cap = ncap;
        }

        c = &cells[count];
        *c = parsed;
        c->ndim = ndim;
        c->dims = malloc((size_t)ndim * sizeof(long));
        c->codes = malloc((size_t)ndim * sizeof(char *));
        if (!c->dims || !c->codes) {
            if (errbuf && errlen)
                snprintf(errbuf, errlen, "out of memory");
            free(c->dims);
            free(c->codes);
            free_quoted(quoted, nq);
            fclose(fd);
            ghm_free_cells(cells, count);
            return -1;
        }
        for (long i = 0; i < ndim; i++) {
            char *end;
            errno = 0;
            c->dims[i] = strtol(quoted[i], &end, 10);
            if (end == quoted[i] || *end || errno == ERANGE || c->dims[i] < 0) {
                if (errbuf && errlen) snprintf(errbuf, errlen, "invalid cell coordinate");
                free(c->dims); free(c->codes);
                free_quoted(quoted, nq);
                fclose(fd);
                ghm_free_cells(cells, count);
                return -1;
            }
        }
        for (long i = 0; i < ndim; i++) {
            c->codes[i] = quoted[ndim + i]; /* take ownership */
        }
        /* The dimension keys were copied via strtol; free their
         * tokens. The code tokens are now owned by c->codes. */
        for (long i = 0; i < ndim; i++)
            free(quoted[i]);
        free(quoted);

        count++;
    }

    if (ferror(fd)) {
        if (errbuf && errlen) snprintf(errbuf, errlen, "error reading cells file");
        fclose(fd);
        ghm_free_cells(cells, count);
        return -1;
    }
    fclose(fd);
    *out = cells;
    *n = count;
    return 0;
}

void ghm_free_cells(ghm_cell *cells, long n)
{
    if (!cells)
        return;
    for (long i = 0; i < n; i++) {
        free(cells[i].dims);
        for (long j = 0; j < cells[i].ndim; j++)
            free(cells[i].codes[j]);
        free(cells[i].codes);
    }
    free(cells);
}
