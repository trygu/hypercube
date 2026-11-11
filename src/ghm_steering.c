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

#include "ghm.h"

/* Read one line, stripping the trailing CR/LF. Returns 0 on success,
 * -1 at end of file. */
static int read_line(FILE *fd, char *buf, size_t len)
{
    if (!fgets(buf, (int)len, fd))
        return -1;
    size_t n = strlen(buf);
    while (n > 0 && (buf[n-1] == '\n' || buf[n-1] == '\r'))
        buf[--n] = '\0';
    return 0;
}

/* Extract the first single-quoted string into out (NUL-terminated,
 * truncated to outlen-1). Returns 0 on success, -1 if none found. */
static int extract_quoted(const char *line, char *out, size_t outlen)
{
    const char *s = strchr(line, '\'');
    if (!s)
        return -1;
    s++;
    const char *e = strchr(s, '\'');
    if (!e)
        return -1;
    size_t len = (size_t)(e - s);
    if (len >= outlen)
        len = outlen - 1;
    memcpy(out, s, len);
    out[len] = '\0';
    return 0;
}

/* Parse up to max whitespace-separated integers from line into vals.
 * Returns how many were parsed. Stops at the first non-numeric token
 * (for example a quoted dimension label). */
static int parse_ints(const char *line, long *vals, int max)
{
    int n = 0;
    const char *p = line;
    while (*p && n < max) {
        while (isspace((unsigned char)*p))
            p++;
        if (!*p)
            break;
        char *end;
        long v = strtol(p, &end, 10);
        if (end == p)
            break;
        vals[n++] = v;
        p = end;
    }
    return n;
}

int ghm_parse_steering(const char *path, ghm_table *out,
                       char *errbuf, size_t errlen)
{
    FILE *fd;
    char line[8192];
    long tmp[256];

    /* Initialise to a released state so the error paths are safe. */
    memset(out, 0, sizeof(*out));

    if (!path || !out) {
        if (errbuf && errlen)
            snprintf(errbuf, errlen, "null argument");
        return -1;
    }

    fd = fopen(path, "r");
    if (!fd) {
        if (errbuf && errlen)
            snprintf(errbuf, errlen, "cannot open steering file: %s", path);
        return -1;
    }

#define FAIL(msg) do { \
        if (errbuf && errlen) snprintf(errbuf, errlen, msg); \
        ghm_free_table(out); \
        fclose(fd); \
        return -1; \
    } while (0)

    /* line 1: the table name (quoted) */
    if (read_line(fd, line, sizeof(line)) < 0)
        FAIL("steering file too short: missing table name");
    extract_quoted(line, out->table_name, sizeof(out->table_name));

    /* line 2: the code read format (quoted); the parser does not use
     * it because it reads the codes token by token. */
    if (read_line(fd, line, sizeof(line)) < 0)
        FAIL("steering file too short: missing format line");

    /* line 3: the number of dimensions */
    if (read_line(fd, line, sizeof(line)) < 0)
        FAIL("steering file too short: missing dimension count");
    {
        int ndim = (int)strtol(line, NULL, 10);
        if (ndim < 1 || ndim > 8)
            FAIL("steering: dimension count out of range");
        out->ndim = ndim;
        out->ms     = calloc((size_t)ndim, sizeof(int));
        out->groups = calloc((size_t)ndim, sizeof(int *));
        out->ncodes = calloc((size_t)ndim, sizeof(int));
        out->codes  = calloc((size_t)ndim, sizeof(long *));
        if (!out->ms || !out->groups || !out->ncodes || !out->codes)
            FAIL("steering: out of memory");
    }

    /* line 4: the number of levels (MS) per dimension */
    if (read_line(fd, line, sizeof(line)) < 0)
        FAIL("steering file too short: missing level counts");
    {
        int n = parse_ints(line, tmp, out->ndim);
        if (n != out->ndim)
            FAIL("steering: expected one level count per dimension");
        for (int i = 0; i < out->ndim; i++) {
            if (tmp[i] < 1 || tmp[i] > 16)
                FAIL("steering: level count out of range");
            out->ms[i] = (int)tmp[i];
        }
    }

    /* lines 5 and 6: the boundary flags (RFLAG) and the distance
     * values (POT); not used by the algorithm in the flat mode-0
     * case, so they are read past. */
    if (read_line(fd, line, sizeof(line)) < 0)
        FAIL("steering file too short: missing boundary flags");
    if (read_line(fd, line, sizeof(line)) < 0)
        FAIL("steering file too short: missing distance values");

    /* one block per dimension: a quoted label, the per-level group
     * counts, then the listed codes. */
    for (int i = 0; i < out->ndim; i++) {
        /* the dimension label (quoted) */
        if (read_line(fd, line, sizeof(line)) < 0)
            FAIL("steering file too short: missing dimension label");

        /* the group counts: one per level (ms[i]) */
        if (read_line(fd, line, sizeof(line)) < 0)
            FAIL("steering file too short: missing group counts");
        {
            int n = parse_ints(line, tmp, out->ms[i]);
            if (n != out->ms[i])
                FAIL("steering: expected one group count per level");
            out->groups[i] = malloc((size_t)out->ms[i] * sizeof(int));
            if (!out->groups[i])
                FAIL("steering: out of memory");
            int total = 0;
            for (int j = 0; j < out->ms[i]; j++) {
                if (tmp[j] < 0)
                    FAIL("steering: negative group count");
                out->groups[i][j] = (int)tmp[j];
                total += (int)tmp[j];
            }
            out->ncodes[i] = total;
            out->codes[i] = total > 0 ?
                malloc((size_t)total * sizeof(long)) : NULL;
            if (total > 0 && !out->codes[i])
                FAIL("steering: out of memory");
        }

        /* the listed codes, several per line */
        {
            int got = 0;
            while (got < out->ncodes[i]) {
                if (read_line(fd, line, sizeof(line)) < 0)
                    FAIL("steering file too short: truncated code list");
                int n = parse_ints(line, tmp, 64);
                for (int j = 0; j < n && got < out->ncodes[i]; j++)
                    out->codes[i][got++] = tmp[j];
            }
        }
    }

    /* the steering tail: the cube ratio (QQUOTE DQUOTE) and then the
     * weighting (GEWRAN SCHAEZ IDBQUA). Both are optional in the
     * legacy reader; parse them when present, else keep the defaults.
     */
    out->qquote = 0.0;
    out->dquote = 0.0;
    out->gewran = 1;
    out->schaez = 1;
    out->idbqua = 1;
    if (read_line(fd, line, sizeof(line)) == 0 && line[0] != '\0')
        sscanf(line, "%lf %lf", &out->qquote, &out->dquote);
    if (read_line(fd, line, sizeof(line)) == 0 && line[0] != '\0')
        sscanf(line, "%d %d %d", &out->gewran, &out->schaez, &out->idbqua);

    fclose(fd);
    return 0;

#undef FAIL
}

void ghm_free_table(ghm_table *t)
{
    if (!t)
        return;
    if (t->ms && t->ndim > 0) {
        for (int i = 0; i < t->ndim; i++) {
            free(t->groups ? t->groups[i] : NULL);
            free(t->codes ? t->codes[i] : NULL);
        }
    }
    free(t->ms);
    free(t->groups);
    free(t->ncodes);
    free(t->codes);
    t->ms = NULL;
    t->groups = NULL;
    t->ncodes = NULL;
    t->codes = NULL;
    t->ndim = 0;
}
