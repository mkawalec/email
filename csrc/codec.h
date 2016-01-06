// Copyright: (c) Magnus Therning, 2012, 2013
// License: BSD3, found in the LICENSE file

#pragma once

#include <stddef.h>
#include <stdint.h>

int qp_dec_c(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t const **rem, size_t *remlen);
