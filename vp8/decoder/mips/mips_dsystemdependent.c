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
#include "vp8/common/blockd.h"
#include "vp8/common/pragmas.h"
#include "vp8/common/postproc.h"
#include "vp8/decoder/dboolhuff.h"
#include "vp8/decoder/dequantize.h"
#include "vp8/decoder/onyxd_int.h"

void vp8_arch_mips_decode_init(VP8D_COMP *pbi)
{
#if CONFIG_RUNTIME_CPU_DETECT

#if (MDSP_REV>=2)

    pbi->dequant.idct_add            = vp8_dequant_idct_add_mips;
    pbi->dequant.dc_idct_add         = vp8_dequant_dc_idct_add_mips;
    pbi->dequant.dc_idct_add_y_block = vp8_dequant_dc_idct_add_y_block_mips;
    pbi->dequant.idct_add_y_block    = vp8_dequant_idct_add_y_block_mips;
    pbi->dequant.idct_add_uv_block   = vp8_dequant_idct_add_uv_block_mips;

#endif
#endif
}
