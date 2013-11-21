LOCAL_PATH := $(call my-dir)
include $(CLEAR_VARS)

ifeq ($(ARCH_ARM_HAVE_NEON),true)
  libvpx_target := armv7a-neon
  libvpx_asm := .asm.s
else ifeq ($(ARCH_ARM_HAVE_ARMV7A),true)
  libvpx_target := armv7a
  libvpx_asm := .asm.s
endif

ifeq ($(TARGET_ARCH),mips)
  ifneq ($(ARCH_HAS_BIGENDIAN),true)
    ifeq ($(ARCH_MIPS_DSP_REV),2)
      libvpx_target := mips-dspr2
    else
      libvpx_target := mips
    endif #mips_dsp_rev2
  endif #bigendian
endif #mips

# Un-optimized targets
libvpx_target ?= generic
libvpx_asm ?= .asm

libvpx_config_dir := $(LOCAL_PATH)/$(libvpx_target)
libvpx_source_dir := $(LOCAL_PATH)/libvpx

libvpx_codec_srcs := $(shell cat $(libvpx_config_dir)/libvpx_srcs.txt)

LOCAL_CFLAGS := -DHAVE_CONFIG_H=vpx_config.h

LOCAL_MODULE := libvpx

LOCAL_MODULE_CLASS := STATIC_LIBRARIES
libvpx_intermediates := $(call local-intermediates-dir)

# Extract the C files from the list and add them to LOCAL_SRC_FILES.
libvpx_codec_srcs_unique := $(sort $(libvpx_codec_srcs))
libvpx_codec_srcs_c := $(filter %.c, $(libvpx_codec_srcs_unique))
# vpx_config.c is an auto-generated file in $(config_dir)
libvpx_codec_srcs_c_static := $(filter-out vpx_config.c, $(libvpx_codec_srcs_c))
LOCAL_SRC_FILES += $(addprefix libvpx/, $(libvpx_codec_srcs_c_static))
LOCAL_SRC_FILES += $(libvpx_target)/vpx_config.c

# ARM and x86 use an 'offsets' file in the assembly. It is generated by
# tricking the compiler and generating non-functional output which is then
# processed with grep. For ARM, this must be additionally converted from
# RVCT (ARM's in-house compiler) format to GNU Assembler Format for gcc.

# Offset files are currently used in vpx_scale for NEON and some encoder
# functions used in both ARM and x86. These files can not be compiled and need
# to be named accordingly to avoid auto-build rules. The encoder files are not
# used yet but are included in the comments for future reference.

libvpx_asm_offsets_intermediates := \
    vp8/encoder/vp8_asm_enc_offsets.intermediate \
    vpx_scale/vpx_scale_asm_offsets.intermediate \

libvpx_asm_offsets_files := \
    vp8/encoder/vp8_asm_enc_offsets.asm \
    vpx_scale/vpx_scale_asm_offsets.asm \

# Build the S files with inline assembly.
COMPILE_TO_S := $(addprefix $(libvpx_intermediates)/, $(libvpx_asm_offsets_intermediates))
$(COMPILE_TO_S) : PRIVATE_INTERMEDIATES := $(libvpx_intermediates)
$(COMPILE_TO_S) : PRIVATE_SOURCE_DIR := $(libvpx_source_dir)
$(COMPILE_TO_S) : PRIVATE_CONFIG_DIR := $(libvpx_config_dir)
$(COMPILE_TO_S) : PRIVATE_CUSTOM_TOOL = $(TARGET_CC) -S $(addprefix -I, $(TARGET_C_INCLUDES)) -I $(PRIVATE_INTERMEDIATES) -I $(PRIVATE_SOURCE_DIR) -I $(PRIVATE_CONFIG_DIR) -DINLINE_ASM -o $@ $<
$(COMPILE_TO_S) : $(libvpx_intermediates)/%.intermediate : $(libvpx_source_dir)/%.c
	$(transform-generated-source)

# Extract the offsets from the inline assembly.
OFFSETS_GEN := $(addprefix $(libvpx_intermediates)/, $(libvpx_asm_offsets_files))
$(OFFSETS_GEN) : PRIVATE_OFFSET_PATTERN := '^[a-zA-Z0-9_]* EQU'
$(OFFSETS_GEN) : PRIVATE_SOURCE_DIR := $(libvpx_source_dir)
$(OFFSETS_GEN) : PRIVATE_CUSTOM_TOOL = grep $(PRIVATE_OFFSET_PATTERN) $< | tr -d '$$\#' | perl $(PRIVATE_SOURCE_DIR)/build/make/ads2gas.pl > $@
$(OFFSETS_GEN) : %.asm : %.intermediate
	$(transform-generated-source)

LOCAL_GENERATED_SOURCES += $(OFFSETS_GEN)

# This step is only required for ARM. MIPS uses intrinsics and x86 requires an
# assembler to pre-process its assembly files.
libvpx_asm_srcs := $(filter %.asm.s, $(libvpx_codec_srcs_unique))

# The ARM assembly sources must be converted from ADS to GAS compatible format.
VPX_GEN := $(addprefix $(libvpx_intermediates)/, $(libvpx_asm_srcs))
$(VPX_GEN) : PRIVATE_SOURCE_DIR := $(libvpx_source_dir)
$(VPX_GEN) : PRIVATE_CUSTOM_TOOL = cat $< | perl $(PRIVATE_SOURCE_DIR)/build/make/ads2gas.pl > $@
$(VPX_GEN) : $(libvpx_intermediates)/%.s : $(libvpx_source_dir)/%
	$(transform-generated-source)

LOCAL_GENERATED_SOURCES += $(VPX_GEN)

LOCAL_C_INCLUDES := \
    $(libvpx_source_dir) \
    $(libvpx_config_dir) \
    $(libvpx_intermediates)/vp8/common \
    $(libvpx_intermediates)/vp8/decoder \
    $(libvpx_intermediates)/vp8/encoder \
    $(libvpx_intermediates)/vpx_scale \

libvpx_target :=
libvpx_asm :=
libvpx_config_dir :=
libvpx_source_dir :=
libvpx_codec_srcs :=
libvpx_intermediates :=
libvpx_codec_srcs_unique :=
libvpx_codec_srcs_c :=
libvpx_codec_srcs_c_static :=
libvpx_asm_offsets_intermediates :=
libvpx_asm_offsets_files :=
libvpx_asm_srcs :=

include $(BUILD_STATIC_LIBRARY)
