/*
 *  Copyright (c) 2010 The VP8 project authors. All Rights Reserved.
 *
 *  Use of this source code is governed by a BSD-style license
 *  that can be found in the LICENSE file in the root of the source
 *  tree. An additional intellectual property rights grant can be found
 *  in the file PATENTS.  All contributing project authors may
 *  be found in the AUTHORS file in the root of the source tree.
 */


#include "vpx_ports/config.h"
#include "onyxc_int.h"

static const unsigned char sub_pel_filters[8][6] =
{
    { 0,  0, 128,   0,  0,  0 },
    { 0,  6, 123,  12,  1,  0 },
    { 2, 11, 108,  36,  8,  1 },         /* New 1/4 pel 6 tap filter */
    { 0,  9,  93,  50,  6,  0 },
    { 3, 16,  77,  77, 16,  3 },         /* New 1/2 pel 6 tap filter */
    { 0,  6,  50,  93,  9,  0 },
    { 1,  8,  36, 108, 11,  2 },         /* New 1/4 pel 6 tap filter */
    { 0,  1,  12, 123,  6,  0 },
};


void vp8_filter_block2d_first_pass_mips
(
    unsigned char *src_ptr,
    unsigned char *output_ptr,
    unsigned int src_pixels_per_line,
    unsigned int output_width,
    unsigned int output_height,
    const unsigned char *vp8_filter,
    int yoffset,
    unsigned char *dst_ptr,
    int pitch
)
{
    unsigned int i, j;
    int Temp1, Temp2, Temp3, Temp4;

    unsigned int vector1a, vector2a, vector3a, vector4a;
    unsigned int vector1b, vector2b, vector3b;

    unsigned int filter0, filter1, filter2;
    unsigned int filter3, filter4, filter5;

    unsigned char src_ptr_l2;
    unsigned char src_ptr_l1;
    unsigned char src_ptr_0;
    unsigned char src_ptr_r1;
    unsigned char src_ptr_r2;
    unsigned char src_ptr_r3;
    unsigned char src_ptr_r4;
    unsigned char src_ptr_r5;
    unsigned char src_ptr_r6;

    filter0 = vp8_filter[0];
    filter1 = vp8_filter[1];
    filter2 = vp8_filter[2];
    filter3 = vp8_filter[3];
    filter4 = vp8_filter[4];
    filter5 = vp8_filter[5];

    vector4a = 0;

    /* if (xoffset == 0) we don'n need any filtering */
    if (vp8_filter[1] == 0) {
        for (i = 0; i < output_height; i++)
        {
            for (j = 0; j < output_width; j+=4)
            {
                output_ptr[j] = (int)src_ptr[0];
                output_ptr[j+1] = (int)src_ptr[1];
                output_ptr[j+2] = (int)src_ptr[2];
                output_ptr[j+3] = (int)src_ptr[3];
                src_ptr += 4;
            }
            /* next row... */
            src_ptr    += src_pixels_per_line - output_width;
            output_ptr += output_width;
        }
    }
    else {

        /* create vectors of filter coefficients */
        __asm__ __volatile__ (
            "sll    %[vector1a], %[filter0],  8             \n\t"
            "sll    %[vector2a], %[filter2],  8             \n\t"
            "sll    %[vector3a], %[filter1],  8             \n\t"
            "or     %[vector1b], %[vector1a], %[filter5]    \n\t"
            "or     %[vector2b], %[vector2a], %[filter3]    \n\t"
            "or     %[vector3b], %[vector3a], %[filter4]    \n\t"
            : [vector1a] "=&r" (vector1a), [vector2a] "=&r" (vector2a),
              [vector3a] "=&r" (vector3a), [vector1b] "=&r" (vector1b),
              [vector2b] "=&r" (vector2b), [vector3b] "=r" (vector3b)
            : [filter0] "r" (filter0), [filter1] "r" (filter1),
              [filter2] "r" (filter2), [filter3] "r" (filter3),
              [filter4] "r" (filter4), [filter5] "r" (filter5)
        );

        /* if (yoffset == 0) don't need temp buffer, data will be stored in dst_ptr */
        if (yoffset == 0) {
            output_height -= 5;
            src_ptr += (src_pixels_per_line + src_pixels_per_line);

            for (i = output_height; i--;)
            {
                /* load first 5 elements for filtering */
                src_ptr_l2 = *(src_ptr-2);
                src_ptr_l1 = *(src_ptr-1);
                src_ptr_0  = *(src_ptr);
                src_ptr_r1 = *(src_ptr+1);
                src_ptr_r2 = *(src_ptr+2);

                /* processing 4 adjacent pixels */
                for (j = 0; j < output_width; j += 4)
                {
                    src_ptr_r3 = *(src_ptr+3);
                    src_ptr_r4 = *(src_ptr+4);
                    src_ptr_r5 = *(src_ptr+5);
                    src_ptr_r6 = *(src_ptr+6);

                    /* apply filter with vectors pairs */
                    __asm__ __volatile__ (
                        "li            %[vector4a],   64                              \n\t"
                        "mtlo          %[vector4a],   $ac0                            \n\t"
                        "mtlo          %[vector4a],   $ac1                            \n\t"
                        "mtlo          %[vector4a],   $ac2                            \n\t"
                        "mtlo          %[vector4a],   $ac3                            \n\t"
                        "sll           %[vector1a],   %[src_ptr_l2], 8                \n\t"
                        "sll           %[vector2a],   %[src_ptr_l1], 8                \n\t"
                        "sll           %[vector3a],   %[src_ptr_0],  8                \n\t"
                        "sll           %[vector4a],   %[src_ptr_r1], 8                \n\t"
                        "or            %[vector1a],   %[vector1a],   %[src_ptr_r3]    \n\t"
                        "or            %[vector2a],   %[vector2a],   %[src_ptr_r4]    \n\t"
                        "or            %[vector3a],   %[vector3a],   %[src_ptr_r5]    \n\t"
                        "or            %[vector4a],   %[vector4a],   %[src_ptr_r6]    \n\t"
                        "dpau.h.qbr    $ac0,          %[vector1a],   %[vector1b]      \n\t"
                        "dpau.h.qbr    $ac1,          %[vector2a],   %[vector1b]      \n\t"
                        "dpau.h.qbr    $ac2,          %[vector3a],   %[vector1b]      \n\t"
                        "dpau.h.qbr    $ac3,          %[vector4a],   %[vector1b]      \n\t"
                        "sll           %[vector1a],   %[src_ptr_0],  8                \n\t"
                        "sll           %[vector2a],   %[src_ptr_r1], 8                \n\t"
                        "sll           %[vector3a],   %[src_ptr_r2], 8                \n\t"
                        "sll           %[vector4a],   %[src_ptr_r3], 8                \n\t"
                        "or            %[vector1a],   %[vector1a],   %[src_ptr_r1]    \n\t"
                        "or            %[vector2a],   %[vector2a],   %[src_ptr_r2]    \n\t"
                        "or            %[vector3a],   %[vector3a],   %[src_ptr_r3]    \n\t"
                        "or            %[vector4a],   %[vector4a],   %[src_ptr_r4]    \n\t"
                        "dpau.h.qbr    $ac0,          %[vector1a],   %[vector2b]      \n\t"
                        "dpau.h.qbr    $ac1,          %[vector2a],   %[vector2b]      \n\t"
                        "dpau.h.qbr    $ac2,          %[vector3a],   %[vector2b]      \n\t"
                        "dpau.h.qbr    $ac3,          %[vector4a],   %[vector2b]      \n\t"
                        "sll           %[vector1a],   %[src_ptr_l1], 8                \n\t"
                        "sll           %[vector2a],   %[src_ptr_0],  8                \n\t"
                        "sll           %[vector3a],   %[src_ptr_r1], 8                \n\t"
                        "sll           %[vector4a],   %[src_ptr_r2], 8                \n\t"
                        "or            %[vector1a],   %[vector1a],   %[src_ptr_r2]    \n\t"
                        "or            %[vector2a],   %[vector2a],   %[src_ptr_r3]    \n\t"
                        "or            %[vector3a],   %[vector3a],   %[src_ptr_r4]    \n\t"
                        "or            %[vector4a],   %[vector4a],   %[src_ptr_r5]    \n\t"
                        "dpsu.h.qbr    $ac0,          %[vector1a],   %[vector3b]      \n\t"
                        "dpsu.h.qbr    $ac1,          %[vector2a],   %[vector3b]      \n\t"
                        "dpsu.h.qbr    $ac2,          %[vector3a],   %[vector3b]      \n\t"
                        "dpsu.h.qbr    $ac3,          %[vector4a],   %[vector3b]      \n\t"
                        "mflo          %[Temp1],      $ac0                            \n\t"
                        "mflo          %[Temp2],      $ac1                            \n\t"
                        "mflo          %[Temp3],      $ac2                            \n\t"
                        "mflo          %[Temp4],      $ac3                            \n\t"

                        : [vector1a] "=&r" (vector1a), [vector2a] "=&r" (vector2a),
                          [vector3a] "=&r" (vector3a), [vector4a] "=&r" (vector4a),
                          [Temp1] "=&r" (Temp1), [Temp2] "=&r" (Temp2),
                          [Temp3] "=&r" (Temp3), [Temp4] "=r" (Temp4)
                        : [vector1b] "r" (vector1b), [vector2b] "r" (vector2b),
                          [vector3b] "r" (vector3b), [src_ptr_l2] "r" (src_ptr_l2),
                          [src_ptr_l1] "r" (src_ptr_l1), [src_ptr_0] "r" (src_ptr_0),
                          [src_ptr_r1] "r" (src_ptr_r1), [src_ptr_r2] "r" (src_ptr_r2),
                          [src_ptr_r3] "r" (src_ptr_r3), [src_ptr_r4] "r" (src_ptr_r4),
                          [src_ptr_r5] "r" (src_ptr_r5), [src_ptr_r6] "r" (src_ptr_r6)
                    );

                    /* don't need load, instead move 5 elements between registers */
                    __asm__ __volatile__ (
                        "addiu         %[src_ptr],    %[src_ptr],    4                \n\t"
                        "move          %[src_ptr_l2], %[src_ptr_r2]                   \n\t"
                        "move          %[src_ptr_l1], %[src_ptr_r3]                   \n\t"
                        "move          %[src_ptr_0],  %[src_ptr_r4]                   \n\t"
                        "move          %[src_ptr_r1], %[src_ptr_r5]                   \n\t"
                        "move          %[src_ptr_r2], %[src_ptr_r6]                   \n\t"
                        : [src_ptr_l2] "=&r" (src_ptr_l2), [src_ptr_l1] "=&r" (src_ptr_l1),
                          [src_ptr_0] "=&r" (src_ptr_0), [src_ptr_r1] "=&r" (src_ptr_r1),
                          [src_ptr_r2] "+r" (src_ptr_r2), [src_ptr] "+r" (src_ptr)
                        : [src_ptr_r3] "r" (src_ptr_r3), [src_ptr_r4] "r" (src_ptr_r4),
                          [src_ptr_r5] "r" (src_ptr_r5), [src_ptr_r6] "r" (src_ptr_r6)
                    );

                    /* clamp to 0 - 255 */
                    if ((signed int)(Temp1) < 0) Temp1 = 0;
                    else {
                        Temp1 >>= 7;
                        if (Temp1 > 255) Temp1 = 255;
                    }

                    if ((signed int)(Temp2) < 0) Temp2 = 0;
                    else {
                        Temp2 >>= 7;
                        if (Temp2 > 255) Temp2 = 255;
                    }

                    if ((signed int)(Temp3) < 0) Temp3 = 0;
                    else {
                        Temp3 >>= 7;
                        if (Temp3 > 255) Temp3 = 255;
                    }

                    if ((signed int)(Temp4) < 0) Temp4 = 0;
                    else {
                        Temp4 >>= 7;
                        if (Temp4 > 255) Temp4 = 255;
                    }

                    dst_ptr[j] = Temp1;
                    dst_ptr[j+1] = Temp2;
                    dst_ptr[j+2] = Temp3;
                    dst_ptr[j+3] = Temp4;
                }

                /* Next row... */
                src_ptr += src_pixels_per_line - output_width;
                dst_ptr += pitch;
            }
        }
        else {
            for (i = output_height; i--;)
            {
                /* load first 5 elements for filtering */
                src_ptr_l2 = *(src_ptr-2);
                src_ptr_l1 = *(src_ptr-1);
                src_ptr_0  = *(src_ptr);
                src_ptr_r1 = *(src_ptr+1);
                src_ptr_r2 = *(src_ptr+2);

                /* processing 4 adjacent pixels */
                for (j = 0; j < output_width; j += 4)
                {
                    src_ptr_r3 = *(src_ptr+3);
                    src_ptr_r4 = *(src_ptr+4);
                    src_ptr_r5 = *(src_ptr+5);
                    src_ptr_r6 = *(src_ptr+6);

                    /* apply filter with vectors pairs */
                    __asm__ __volatile__ (
                        "li            %[vector4a],   64                              \n\t"
                        "mtlo          %[vector4a],   $ac0                            \n\t"
                        "mtlo          %[vector4a],   $ac1                            \n\t"
                        "mtlo          %[vector4a],   $ac2                            \n\t"
                        "mtlo          %[vector4a],   $ac3                            \n\t"
                        "sll           %[vector1a],   %[src_ptr_l2], 8                \n\t"
                        "sll           %[vector2a],   %[src_ptr_l1], 8                \n\t"
                        "sll           %[vector3a],   %[src_ptr_0],  8                \n\t"
                        "sll           %[vector4a],   %[src_ptr_r1], 8                \n\t"
                        "or            %[vector1a],   %[vector1a],   %[src_ptr_r3]    \n\t"
                        "or            %[vector2a],   %[vector2a],   %[src_ptr_r4]    \n\t"
                        "or            %[vector3a],   %[vector3a],   %[src_ptr_r5]    \n\t"
                        "or            %[vector4a],   %[vector4a],   %[src_ptr_r6]    \n\t"
                        "dpau.h.qbr    $ac0,          %[vector1a],   %[vector1b]      \n\t"
                        "dpau.h.qbr    $ac1,          %[vector2a],   %[vector1b]      \n\t"
                        "dpau.h.qbr    $ac2,          %[vector3a],   %[vector1b]      \n\t"
                        "dpau.h.qbr    $ac3,          %[vector4a],   %[vector1b]      \n\t"
                        "sll           %[vector1a],   %[src_ptr_0],  8                \n\t"
                        "sll           %[vector2a],   %[src_ptr_r1], 8                \n\t"
                        "sll           %[vector3a],   %[src_ptr_r2], 8                \n\t"
                        "sll           %[vector4a],   %[src_ptr_r3], 8                \n\t"
                        "or            %[vector1a],   %[vector1a],   %[src_ptr_r1]    \n\t"
                        "or            %[vector2a],   %[vector2a],   %[src_ptr_r2]    \n\t"
                        "or            %[vector3a],   %[vector3a],   %[src_ptr_r3]    \n\t"
                        "or            %[vector4a],   %[vector4a],   %[src_ptr_r4]    \n\t"
                        "dpau.h.qbr    $ac0,          %[vector1a],   %[vector2b]      \n\t"
                        "dpau.h.qbr    $ac1,          %[vector2a],   %[vector2b]      \n\t"
                        "dpau.h.qbr    $ac2,          %[vector3a],   %[vector2b]      \n\t"
                        "dpau.h.qbr    $ac3,          %[vector4a],   %[vector2b]      \n\t"
                        "sll           %[vector1a],   %[src_ptr_l1], 8                \n\t"
                        "sll           %[vector2a],   %[src_ptr_0],  8                \n\t"
                        "sll           %[vector3a],   %[src_ptr_r1], 8                \n\t"
                        "sll           %[vector4a],   %[src_ptr_r2], 8                \n\t"
                        "or            %[vector1a],   %[vector1a],   %[src_ptr_r2]    \n\t"
                        "or            %[vector2a],   %[vector2a],   %[src_ptr_r3]    \n\t"
                        "or            %[vector3a],   %[vector3a],   %[src_ptr_r4]    \n\t"
                        "or            %[vector4a],   %[vector4a],   %[src_ptr_r5]    \n\t"
                        "dpsu.h.qbr    $ac0,          %[vector1a],   %[vector3b]      \n\t"
                        "dpsu.h.qbr    $ac1,          %[vector2a],   %[vector3b]      \n\t"
                        "dpsu.h.qbr    $ac2,          %[vector3a],   %[vector3b]      \n\t"
                        "dpsu.h.qbr    $ac3,          %[vector4a],   %[vector3b]      \n\t"
                        "mflo          %[Temp1],      $ac0                            \n\t"
                        "mflo          %[Temp2],      $ac1                            \n\t"
                        "mflo          %[Temp3],      $ac2                            \n\t"
                        "mflo          %[Temp4],      $ac3                            \n\t"

                        : [vector1a] "=&r" (vector1a), [vector2a] "=&r" (vector2a),
                          [vector3a] "=&r" (vector3a), [vector4a] "=&r" (vector4a),
                          [Temp1] "=&r" (Temp1), [Temp2] "=&r" (Temp2),
                          [Temp3] "=&r" (Temp3), [Temp4] "=r" (Temp4)
                        : [vector1b] "r" (vector1b), [vector2b] "r" (vector2b),
                          [vector3b] "r" (vector3b), [src_ptr_l2] "r" (src_ptr_l2),
                          [src_ptr_l1] "r" (src_ptr_l1), [src_ptr_0] "r" (src_ptr_0),
                          [src_ptr_r1] "r" (src_ptr_r1), [src_ptr_r2] "r" (src_ptr_r2),
                          [src_ptr_r3] "r" (src_ptr_r3), [src_ptr_r4] "r" (src_ptr_r4),
                          [src_ptr_r5] "r" (src_ptr_r5), [src_ptr_r6] "r" (src_ptr_r6)
                    );

                    /* don't need load, instead move 5 elements between registers */
                    __asm__ __volatile__ (
                        "addiu         %[src_ptr],    %[src_ptr],    4                \n\t"
                        "move          %[src_ptr_l2], %[src_ptr_r2]                   \n\t"
                        "move          %[src_ptr_l1], %[src_ptr_r3]                   \n\t"
                        "move          %[src_ptr_0],  %[src_ptr_r4]                   \n\t"
                        "move          %[src_ptr_r1], %[src_ptr_r5]                   \n\t"
                        "move          %[src_ptr_r2], %[src_ptr_r6]                   \n\t"
                        : [src_ptr_l2] "=&r" (src_ptr_l2), [src_ptr_l1] "=&r" (src_ptr_l1),
                          [src_ptr_0] "=&r" (src_ptr_0), [src_ptr_r1] "=&r" (src_ptr_r1),
                          [src_ptr_r2] "+r" (src_ptr_r2), [src_ptr] "+r" (src_ptr)
                        : [src_ptr_r3] "r" (src_ptr_r3), [src_ptr_r4] "r" (src_ptr_r4),
                          [src_ptr_r5] "r" (src_ptr_r5), [src_ptr_r6] "r" (src_ptr_r6)
                    );

                    /* clamp to 0 - 255 */
                    if ((signed int)(Temp1) < 0) Temp1 = 0;
                    else {
                        Temp1 >>= 7;
                        if (Temp1 > 255) Temp1 = 255;
                    }

                    if ((signed int)(Temp2) < 0) Temp2 = 0;
                    else {
                        Temp2 >>= 7;
                        if (Temp2 > 255) Temp2 = 255;
                    }

                    if ((signed int)(Temp3) < 0) Temp3 = 0;
                    else {
                        Temp3 >>= 7;
                        if (Temp3 > 255) Temp3 = 255;
                    }

                    if ((signed int)(Temp4) < 0) Temp4 = 0;
                    else {
                        Temp4 >>= 7;
                        if (Temp4 > 255) Temp4 = 255;
                    }

                    output_ptr[j] = Temp1;
                    output_ptr[j+1] = Temp2;
                    output_ptr[j+2] = Temp3;
                    output_ptr[j+3] = Temp4;
                }

                /* next row... */
                src_ptr += src_pixels_per_line - output_width;
                output_ptr += output_width;
            }
        }
    }
}


void vp8_filter_block2d_second_pass_mips
(
    unsigned char *src_ptr,
    unsigned char *output_ptr,
    int output_pitch,
    unsigned int output_height,
    unsigned int output_width,
    const unsigned char *vp8_filter
)
{
    unsigned int i, j, ow2;

    int Temp1, Temp2;
    unsigned int vector1a, vector2a, vector3a, vector4a;
    unsigned int vector1b, vector2b, vector3b;

    unsigned int filter0, filter1, filter2;
    unsigned int filter3, filter4, filter5;

    unsigned char src_ptr_l2;
    unsigned char src_ptr_l1;
    unsigned char src_ptr_0;
    unsigned char src_ptr_r1;
    unsigned char src_ptr_r2;
    unsigned char src_ptr_r3;

    filter0 = vp8_filter[0];
    filter1 = vp8_filter[1];
    filter2 = vp8_filter[2];
    filter3 = vp8_filter[3];
    filter4 = vp8_filter[4];
    filter5 = vp8_filter[5];

    vector4a = 0;
    ow2 = output_width + output_width;

    /* create vectors of filter coefficients */
    __asm__ __volatile__ (
        "sll    %[vector1a], %[filter0],  8             \n\t"
        "sll    %[vector2a], %[filter2],  8             \n\t"
        "sll    %[vector3a], %[filter1],  8             \n\t"
        "or     %[vector1b], %[vector1a], %[filter5]    \n\t"
        "or     %[vector2b], %[vector2a], %[filter3]    \n\t"
        "or     %[vector3b], %[vector3a], %[filter4]    \n\t"
        : [vector1a] "=&r" (vector1a), [vector2a] "=&r" (vector2a),
          [vector3a] "=&r" (vector3a), [vector1b] "=&r" (vector1b),
          [vector2b] "=&r" (vector2b), [vector3b] "=r" (vector3b)
        : [filter0] "r" (filter0), [filter1] "r" (filter1),
          [filter2] "r" (filter2), [filter3] "r" (filter3),
          [filter4] "r" (filter4), [filter5] "r" (filter5)
    );

    for (i = output_height; i--;)
    {
        /* unrolling for loop */
        for (j = 0; j < output_width; j += 2)
        {
            /* load elements for filtering */
            src_ptr_l2 = src_ptr[-ow2];
            src_ptr_l1 = src_ptr[-output_width];
            src_ptr_0  = src_ptr[0];
            src_ptr_r1 = src_ptr[output_width];
            src_ptr_r2 = src_ptr[ow2];
            src_ptr_r3 = src_ptr[ow2 + output_width];

            /* apply filter with vectors pairs */
            __asm__ __volatile__ (
                "li            %[vector4a], 64                              \n\t"
                "mtlo          %[vector4a], $ac2                            \n\t"
                "sll           %[vector1a], %[src_ptr_l2], 8                \n\t"
                "sll           %[vector2a], %[src_ptr_0],  8                \n\t"
                "sll           %[vector3a], %[src_ptr_l1], 8                \n\t"
                "or            %[vector1a], %[vector1a],   %[src_ptr_r3]    \n\t"
                "or            %[vector2a], %[vector2a],   %[src_ptr_r1]    \n\t"
                "or            %[vector3a], %[vector3a],   %[src_ptr_r2]    \n\t"
                "dpau.h.qbr    $ac2,        %[vector1a],   %[vector1b]      \n\t"
                "dpau.h.qbr    $ac2,        %[vector2a],   %[vector2b]      \n\t"
                "dpsu.h.qbr    $ac2,        %[vector3a],   %[vector3b]      \n\t"
                "mflo          %[Temp1],    $ac2                            \n\t"

                : [vector1a] "=&r" (vector1a), [vector2a] "=&r" (vector2a),
                  [vector3a] "=&r" (vector3a), [vector4a] "=&r" (vector4a),
                  [Temp1] "=r" (Temp1)
                : [vector1b] "r" (vector1b), [vector2b] "r" (vector2b),
                  [vector3b] "r" (vector3b), [src_ptr_l2] "r" (src_ptr_l2),
                  [src_ptr_l1] "r" (src_ptr_l1), [src_ptr_0] "r" (src_ptr_0),
                  [src_ptr_r1] "r" (src_ptr_r1), [src_ptr_r2] "r" (src_ptr_r2),
                  [src_ptr_r3] "r" (src_ptr_r3)
            );

            /* load elements for filtering */
            src_ptr_l2 = src_ptr[-ow2+1];
            src_ptr_l1 = src_ptr[-output_width+1];
            src_ptr_0  = src_ptr[1];
            src_ptr_r1 = src_ptr[output_width+1];
            src_ptr_r2 = src_ptr[ow2+1];
            src_ptr_r3 = src_ptr[ow2 + output_width+1];

            /* apply filter with vectors pairs */
            __asm__ __volatile__ (
                "li            %[vector4a], 64                              \n\t"
                "mtlo          %[vector4a], $ac3                            \n\t"
                "sll           %[vector1a], %[src_ptr_l2], 8                \n\t"
                "sll           %[vector2a], %[src_ptr_0],  8                \n\t"
                "sll           %[vector3a], %[src_ptr_l1], 8                \n\t"
                "or            %[vector1a], %[vector1a],   %[src_ptr_r3]    \n\t"
                "or            %[vector2a], %[vector2a],   %[src_ptr_r1]    \n\t"
                "or            %[vector3a], %[vector3a],   %[src_ptr_r2]    \n\t"
                "dpau.h.qbr    $ac3,        %[vector1a],   %[vector1b]      \n\t"
                "dpau.h.qbr    $ac3,        %[vector2a],   %[vector2b]      \n\t"
                "dpsu.h.qbr    $ac3,        %[vector3a],   %[vector3b]      \n\t"
                "mflo          %[Temp2],    $ac3                            \n\t"

                : [vector1a] "=&r" (vector1a), [vector2a] "=&r" (vector2a),
                  [vector3a] "=&r" (vector3a), [vector4a] "=&r" (vector4a),
                  [Temp2] "=r" (Temp2)
                : [vector1b] "r" (vector1b), [vector2b] "r" (vector2b),
                  [vector3b] "r" (vector3b), [src_ptr_l2] "r" (src_ptr_l2),
                  [src_ptr_l1] "r" (src_ptr_l1), [src_ptr_0] "r" (src_ptr_0),
                  [src_ptr_r1] "r" (src_ptr_r1), [src_ptr_r2] "r" (src_ptr_r2),
                  [src_ptr_r3] "r" (src_ptr_r3)
            );

            /* clamp to 0 - 255 */
            if ((signed int)(Temp1) < 0) Temp1 = 0;
            else {
                Temp1 >>= 7;
                if (Temp1 > 255) Temp1 = 255;
            }

            if ((signed int)(Temp2) < 0) Temp2 = 0;
            else {
                Temp2 >>= 7;
                if (Temp2 > 255) Temp2 = 255;
            }

            output_ptr[j] = (unsigned char)Temp1;
            output_ptr[j+1] = (unsigned char)Temp2;

            src_ptr += 2;
        }

        /* Start next row */
        output_ptr += output_pitch;
    }
}


void vp8_filter_block2d_mips
(
    unsigned char  *src_ptr,
    unsigned char  *output_ptr,
    unsigned int src_pixels_per_line,
    int output_pitch,
    const unsigned char *HFilter,
    const unsigned char *VFilter,
    int yoffset
)
{
    unsigned char FData[9*4]; /* Temp data bufffer used in filtering */

    /* First filter 1-D horizontally... */
    vp8_filter_block2d_first_pass_mips(src_ptr - (2 * src_pixels_per_line), FData, src_pixels_per_line,
                                       4, 9, HFilter, yoffset, output_ptr, output_pitch);

    /* then filter verticaly...
     * if (yoffsset == 0) vp8_filter_block2d_first_pass save data to output_ptr
     */
    if (yoffset) vp8_filter_block2d_second_pass_mips(FData + 8, output_ptr, output_pitch, 4, 4, VFilter);
}


void vp8_sixtap_predict_mips
(
    unsigned char  *src_ptr,
    int   src_pixels_per_line,
    int  xoffset,
    int  yoffset,
    unsigned char *dst_ptr,
    int dst_pitch
)
{
    const unsigned char  *HFilter;
    const unsigned char  *VFilter;

    HFilter = sub_pel_filters[xoffset];   /* 6 tap */
    VFilter = sub_pel_filters[yoffset];   /* 6 tap */

    vp8_filter_block2d_mips(src_ptr, dst_ptr, src_pixels_per_line, dst_pitch, HFilter, VFilter, yoffset);
}


void vp8_sixtap_predict8x8_mips
(
    unsigned char  *src_ptr,
    int  src_pixels_per_line,
    int  xoffset,
    int  yoffset,
    unsigned char *dst_ptr,
    int  dst_pitch
)
{
    const unsigned char  *HFilter;
    const unsigned char  *VFilter;
    unsigned char FData[13*8];   /* Temp data bufffer used in filtering */

    HFilter = sub_pel_filters[xoffset];   /* 6 tap */
    VFilter = sub_pel_filters[yoffset];   /* 6 tap */

    /* First filter 1-D horizontally... */
    vp8_filter_block2d_first_pass_mips(src_ptr - (2 * src_pixels_per_line), FData, src_pixels_per_line,
                                       8, 13, HFilter, yoffset, dst_ptr, dst_pitch);

    /* then filter verticaly...
     * if (yoffsset == 0) vp8_filter_block2d_first_pass save data to dst_ptr
     */
    if (yoffset) vp8_filter_block2d_second_pass_mips(FData + 16, dst_ptr, dst_pitch, 8, 8, VFilter);
}


void vp8_sixtap_predict8x4_mips
(
    unsigned char  *src_ptr,
    int  src_pixels_per_line,
    int  xoffset,
    int  yoffset,
    unsigned char *dst_ptr,
    int  dst_pitch
)
{
    const unsigned char  *HFilter;
    const unsigned char  *VFilter;
    unsigned char FData[13*8];   /* Temp data bufffer used in filtering */

    HFilter = sub_pel_filters[xoffset];   /* 6 tap */
    VFilter = sub_pel_filters[yoffset];   /* 6 tap */

    /* First filter 1-D horizontally... */
    vp8_filter_block2d_first_pass_mips(src_ptr - (2 * src_pixels_per_line), FData, src_pixels_per_line,
                                       8, 9, HFilter, yoffset, dst_ptr, dst_pitch);

    /* then filter verticaly...
     * if (yoffsset == 0) vp8_filter_block2d_first_pass save data to dst_ptr
     */
    if (yoffset) vp8_filter_block2d_second_pass_mips(FData + 16, dst_ptr, dst_pitch, 4, 8, VFilter);
}


void vp8_sixtap_predict16x16_mips
(
    unsigned char  *src_ptr,
    int  src_pixels_per_line,
    int  xoffset,
    int  yoffset,
    unsigned char *dst_ptr,
    int  dst_pitch
)
{
    const unsigned char  *HFilter;
    const unsigned char  *VFilter;
    unsigned char FData[21*16];   /* Temp data bufffer used in filtering */

    HFilter = sub_pel_filters[xoffset];   /* 6 tap */
    VFilter = sub_pel_filters[yoffset];   /* 6 tap */

    /* First filter 1-D horizontally... */
    vp8_filter_block2d_first_pass_mips(src_ptr - (2 * src_pixels_per_line), FData, src_pixels_per_line,
                                       16, 21, HFilter, yoffset, dst_ptr, dst_pitch);

    /* then filter verticaly...
     * if (yoffsset == 0) vp8_filter_block2d_first_pass save data to dst_ptr
     */
    if (yoffset) vp8_filter_block2d_second_pass_mips(FData + 32, dst_ptr, dst_pitch, 16, 16, VFilter);
}
