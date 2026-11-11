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

#include "ghm.h"

/*
 * Collect every single-quoted token in a line into a growing array.
 * Returns the number of tokens stored, or -1 on allocation failure
 * (the partially filled array is left in *out; the caller must not
 * rely on freeing it in that case, an OOM path). Each token is a
 * malloc'd NUL-terminated string without the surrounding quotes.
 */
static int collect_quoted(const char *line, char ***out, int *cap)
{
    int i = 0;
    const char *p = line;
    while ((p = strchr(p, '\'')) != NULL) {
        const char *start = p + 1;
        const char *end = strchr(start, '\'');
        if (end == NULL)
            break; /* unbalanced quote; stop */
        if (i == *cap) {
            int ncap = *cap ? *cap * 2 : 8;
            char **tmp = realloc(*out, (size_t)ncap * sizeof(**out));
            if (!tmp)
                return -1;
            *out = tmp;
            *cap = ncap;
        }
        size_t len = (size_t)(end - start);
        char *tok = malloc(len + 1);
        if (!tok)
            return -1;
        memcpy(tok, start, len);
        tok[len] = '\0';
        (*out)[i++] = tok;
        p = end + 1;
    }
    return i;
}

static void free_quoted(char **quoted, int n)
{
    if (!quoted)
        return;
    for (int i = 0; i < n; i++)
        free(quoted[i]);
    free(quoted);
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
        int wertart, gewi, hoew;
        long freq;
        double resp, fehl0, fehl1;
        char **quoted = NULL;
        int qcap = 0, nq;
        long ndim;
        ghm_cell *c;

        /* Skip blank lines. */
        {
            const char *k = line;
            while (*k == ' ' || *k == '\t')
                k++;
            if (*k == '\n' || *k == '\r' || *k == '\0')
                continue;
        }

        if (sscanf(line, "%d %ld %lf %d %d %lf %lf",
                   &wertart, &freq, &resp, &gewi, &hoew, &fehl0, &fehl1) != 7) {
            if (errbuf && errlen)
                snprintf(errbuf, errlen, "malformed cell record: %s", line);
            fclose(fd);
            free(cells);
            return -1;
        }

        nq = collect_quoted(line, &quoted, &qcap);
        if (nq < 0 || (nq % 2) != 0) {
            if (errbuf && errlen)
                snprintf(errbuf, errlen,
                         "expected an even number of quoted tokens, got %d",
                         nq);
            if (nq >= 0)
                free_quoted(quoted, nq);
            fclose(fd);
            free(cells);
            return -1;
        }
        ndim = nq / 2;

        if (count == cap) {
            long ncap = cap ? cap * 2 : 16;
            ghm_cell *tmp = realloc(cells, (size_t)ncap * sizeof(*cells));
            if (!tmp) {
                if (errbuf && errlen)
                    snprintf(errbuf, errlen, "out of memory");
                free_quoted(quoted, nq);
                fclose(fd);
                free(cells);
                return -1;
            }
            cells = tmp;
            cap = ncap;
        }

        c = &cells[count];
        c->wertart = wertart;
        c->freq = freq;
        c->resp = resp;
        c->gewi = gewi;
        c->hoew = hoew;
        c->fehl0 = fehl0;
        c->fehl1 = fehl1;
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
            free(cells);
            return -1;
        }
        for (long i = 0; i < ndim; i++) {
            c->dims[i] = strtol(quoted[i], NULL, 10);
            c->codes[i] = quoted[ndim + i]; /* take ownership */
        }
        /* The dimension keys were copied via strtol; free their
         * tokens. The code tokens are now owned by c->codes. */
        for (long i = 0; i < ndim; i++)
            free(quoted[i]);
        free(quoted);

        count++;
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
