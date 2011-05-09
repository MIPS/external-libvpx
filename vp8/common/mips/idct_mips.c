/*
 *  Copyright (c) 2010 The WebM project authors. All Rights Reserved.
 *
 *  Use of this source code is governed by a BSD-style license
 *  that can be found in the LICENSE file in the root of the source
 *  tree. An additional intellectual property rights grant can be found
 *  in the file PATENTS.  All contributing project authors may
 *  be found in the AUTHORS file in the root of the source tree.
 */


#include "vpx_ports/config.h"
#include "onyxc_int.h"

void vp8_dc_only_idct_add_mips
(
    short input_dc,
    unsigned char *pred_ptr,
    unsigned char *dst_ptr,
    int pitch,
    int stride
)
{
    int i, a1, absa1;
    int t2, vector_a1, vector_a;

    /* a1 = ((input_dc + 4) >> 3); */
    __asm__ __volatile__ (
        "addi  %[a1], %[input_dc], 4   \n\t"
        "sra   %[a1], %[a1],       3   \n\t"
        : [a1] "=r" (a1)
        : [input_dc] "r" (input_dc)
    );

    /* first for loop is unrolled
     * if (a1 < 0) then always (a1 + pred_ptr[c]) < 255
     */
    if (a1 < 0) {
        /* use quad-byte
         * input and output memory are four byte aligned
         */
        __asm__ __volatile__ (
            "abs        %[absa1],     %[a1]         \n\t"
            "replv.qb   %[vector_a1], %[absa1]      \n\t"
            : [absa1] "=r" (absa1), [vector_a1] "=r" (vector_a1)
            : [a1] "r" (a1)
        );

        /* use (a1 - predptr[c]) instead a1 + predptr[c] */
        for (i = 4; i--;)
        {
            __asm__ __volatile__ (
                "lw             %[t2],       0(%[pred_ptr])                     \n\t"
                "add            %[pred_ptr], %[pred_ptr],    %[pitch]           \n\t"
                "subu_s.qb      %[vector_a], %[t2],          %[vector_a1]       \n\t"
                "sw             %[vector_a], 0(%[dst_ptr])                      \n\t"
                "add            %[dst_ptr],  %[dst_ptr],     %[stride]          \n\t"
                : [t2] "=&r" (t2), [vector_a] "=&r" (vector_a),
                  [dst_ptr] "+&r" (dst_ptr), [pred_ptr] "+&r" (pred_ptr)
                : [stride] "r" (stride), [pitch] "r" (pitch), [vector_a1] "r" (vector_a1)
            );
        }
    }
    else {
        /* use quad-byte
         * input and output memory are four byte aligned
         */
        __asm__ __volatile__ (
            "replv.qb       %[vector_a1], %[a1]     \n\t"
            : [vector_a1] "=r" (vector_a1)
            : [a1] "r" (a1)
        );

        for (i = 4; i--;)
        {
            __asm__ __volatile__ (
                "lw             %[t2],       0(%[pred_ptr])                 \n\t"
                "add            %[pred_ptr], %[pred_ptr],    %[pitch]       \n\t"
                "addu_s.qb      %[vector_a], %[vector_a1],   %[t2]          \n\t"
                "sw             %[vector_a], 0(%[dst_ptr])                  \n\t"
                "add            %[dst_ptr],  %[dst_ptr],     %[stride]      \n\t"
                : [t2] "=&r" (t2), [vector_a] "=&r" (vector_a),
                  [dst_ptr] "+&r" (dst_ptr), [pred_ptr] "+&r" (pred_ptr)
                : [stride] "r" (stride), [pitch] "r" (pitch), [vector_a1] "r" (vector_a1)
            );
        }
    }
}


void vp8_short_inv_walsh4x4_1_mips(short *input, short *output)
{
    int a1;
    int vect_a;
    unsigned int *op = (unsigned int *)output;

    a1 = ((input[0] + 3) >> 3);

    __asm__ __volatile__ (
        "replv.ph   %[vect_a], %[a1]    \n\t"
        : [vect_a] "=r" (vect_a)
        : [a1] "r" (a1)
    );

    /* output is 4 byte aligned */
    op[0] = vect_a;
    op[1] = vect_a;
    op[2] = vect_a;
    op[3] = vect_a;
    op[4] = vect_a;
    op[5] = vect_a;
    op[6] = vect_a;
    op[7] = vect_a;
}