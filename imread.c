/* PHOROS -- Photogrammetric Road Survey */
/* Copyright (C) 2016, 2017 Bert Burgemeister */

/* This program is free software; you can redistribute it and/or modify */
/* it under the terms of the GNU General Public License as published by */
/* the Free Software Foundation; either version 2 of the License, or */
/* (at your option) any later version. */

/* This program is distributed in the hope that it will be useful, */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the */
/* GNU General Public License for more details. */

/* You should have received a copy of the GNU General Public License along */
/* with this program; if not, write to the Free Software Foundation, Inc., */
/* 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA. */

#include <malloc.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include <png.h>
#include <jpeglib.h>

enum color {RED, GREEN, BLUE};

/* 
   Set one particular color of the cell in row at position x to the
   average of the surrounding values found in rawp
 */
void
cplt_horiz(unsigned char *rawp, png_bytep row, int y, int x, int color, int width)
{
        (void) y;
        (void) width;
        row[3 * x + color] = (rawp[-1] + rawp[1]) / 2;
}

void
cplt_vert(unsigned char *rawp, png_bytep row, int y, int x, int color, int width)
{
        (void) y;
        row[3 * x + color] = (rawp[-width] + rawp[width]) / 2;
}

void
cplt_squ(unsigned char *rawp, png_bytep row, int y, int x, int color, int width)
{
        (void) y;
        row[3 * x + color] = (rawp[-1] + rawp[1] + rawp[-width] + rawp[width]) / 4;
}

void
cplt_diag(unsigned char *rawp, png_bytep row, int y, int x, int color, int width)
{
        (void) y;
        row[3 * x  + color] = (rawp[-width - 1] + rawp[-width + 1] +
                               rawp[width - 1] + rawp[width + 1]) / 4;
}

/* 
    Add missing colors to a green cell in a red and green row
 */
void
cplt_g_on_r(unsigned char *rawp, png_bytep row, int y, int x, int width)
{
        cplt_horiz(rawp, row, y, x, RED, width);
        cplt_vert(rawp, row, y, x, BLUE, width);
}

/* 
    Add missing colors to a green cell in a blue and green row
 */
void
cplt_g_on_b(unsigned char *rawp, png_bytep row, int y, int x, int width)
{
        cplt_horiz(rawp, row, y, x, BLUE, width);
        cplt_vert(rawp, row, y, x, RED, width);
}

/* 
    Add missing colors to a red cell
 */
void
cplt_r(unsigned char *rawp, png_bytep row, int y, int x, int width)
{
        cplt_squ(rawp, row, y, x, GREEN, width);
        cplt_diag(rawp, row, y, x, BLUE, width);
}

/* 
    Add missing colors to a blue cell
 */
void
cplt_b(unsigned char *rawp, png_bytep row, int y, int x, int width)
{
        cplt_squ(rawp, row, y, x, GREEN, width);
        cplt_diag(rawp, row, y, x, RED, width);
}

/*
   Set one particular color of the cell in row at position x to the
   value found in rawp
 */
void
colrz_r(unsigned char *rawp, png_bytep row, int y, int x)
{
        (void) y;
        row[3 * x + RED] = rawp[0];
}
        
void
colrz_g(unsigned char *rawp, png_bytep row, int y, int x)
{
        (void) y;
        row[3 * x + GREEN] = rawp[0];
}
        
void
colrz_b(unsigned char *rawp, png_bytep row, int y, int x)
{
        (void) y;
        row[3 * x + BLUE] = rawp[0];
}

void
raise_color(png_bytep row, int x, double *colr_raisr)
{
        int i, val;
        
        for (i = 0; i < 3; i++) {
                val = round(row[3 * x + i] * colr_raisr[i]);
                if (val > 255)
                        val = 255;
                row[3 * x + i] = val;
        }
}
        
void
raise_noop(png_bytep row, int x, double *colr_raisr)
{
        (void) row;
        (void) x;
        (void) colr_raisr;
}

/* Storage for the in-memory PNG */
struct mem_encode
{
        char *buffer;
        size_t size;
};

void
write_png_data(png_structp png_ptr, png_bytep data, png_size_t length)
{
        struct mem_encode *p = (struct mem_encode *)png_get_io_ptr(png_ptr);
        size_t nsize = p->size + length;
        /* allocate or grow buffer */
        if (p->buffer)
                p->buffer = realloc(p->buffer, nsize);
        else
                p->buffer = malloc(nsize);
        if (!p->buffer)
                png_error(png_ptr, "Write Error");
        /* copy new bytes to end of buffer */
        memcpy(p->buffer + p->size, data, length);
        p->size += length;
}

void
flush_png(png_structp png_ptr)
{
        (void) png_ptr;
}

int
uncompressed2png(struct mem_encode *mem_png, int width, int height, int channels,
                 int *bayerpat, double *color_raiser, unsigned char *uncompressed)
{
	volatile int retval = 0; /* silence -Wclobbered */
	png_structp png_ptr = NULL;
	png_infop info_ptr = NULL;
	volatile png_bytep row = NULL; /* silence -Wclobbered */
        void (*colrz_ev_ev)(unsigned char *, png_bytep, int, int) = NULL;
        void (*colrz_ev_od)(unsigned char *, png_bytep, int, int) = NULL;
        void (*colrz_od_ev)(unsigned char *, png_bytep, int, int) = NULL;
        void (*colrz_od_od)(unsigned char *, png_bytep, int, int) = NULL;
        void (*cplt_ev_ev)(unsigned char *, png_bytep, int, int, int) = NULL;
        void (*cplt_ev_od)(unsigned char *, png_bytep, int, int, int) = NULL;
        void (*cplt_od_ev)(unsigned char *, png_bytep, int, int, int) = NULL;
        void (*cplt_od_od)(unsigned char *, png_bytep, int, int, int) = NULL;
        int x, y, x_ev, x_od, y_ev, y_od;
        int i;

        png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
	if (png_ptr == NULL) {
		retval = 11; /* Could not allocate write struct */
		goto finalize;
	}
	info_ptr = png_create_info_struct(png_ptr);
	if (info_ptr == NULL) {
		retval = 12; /* Could not allocate info struct */
		goto finalize;
	}
	if (setjmp(png_jmpbuf(png_ptr))) {
		retval = 13; /* Error during png creation */
		goto finalize;
	}
        mem_png->buffer = NULL;
        mem_png->size = 0;
        png_set_write_fn(png_ptr, mem_png, write_png_data, flush_png);
	row = malloc(channels * width * sizeof(png_byte));
        if (row == NULL) {
                retval = 21;
                goto finalize;
        }
        if (channels == 3) {
                void (*raise)(unsigned char *, int, double *) = raise_noop;
                
                for (i = 0; i < 3; i++)
                        if (fabs(color_raiser[i] - 1) > 0.00001)
                                raise = raise_color;
                if (bayerpat[0] == 0x0000ff) { /* red */
                        colrz_ev_ev = colrz_r;
                        colrz_ev_od = colrz_g;
                        colrz_od_ev = colrz_g;
                        colrz_od_od = colrz_b;
                        cplt_ev_ev = cplt_r;
                        cplt_ev_od = cplt_g_on_r;
                        cplt_od_ev = cplt_g_on_b;
                        cplt_od_od = cplt_b;
                } else if (bayerpat[0] == 0xff0000) { /* blue */
                        colrz_ev_ev = colrz_b;
                        colrz_ev_od = colrz_g;
                        colrz_od_ev = colrz_g;
                        colrz_od_od = colrz_r;
                        cplt_ev_ev = cplt_b;
                        cplt_ev_od = cplt_g_on_b;
                        cplt_od_ev = cplt_g_on_r;
                        cplt_od_od = cplt_b;
                } else if (bayerpat[0] == 0x00ff00) { /* green */
                        if (bayerpat[1] == 0x0000ff) { /* red */
                                colrz_ev_ev = colrz_g;
                                colrz_ev_od = colrz_r;
                                colrz_od_ev = colrz_b;
                                colrz_od_od = colrz_g;
                                cplt_ev_ev = cplt_g_on_r;
                                cplt_ev_od = cplt_r;
                                cplt_od_ev = cplt_b;
                                cplt_od_od = cplt_g_on_b;
                        } else if (bayerpat[1] == 0xff0000) { /* blue */
                                colrz_ev_ev = colrz_g;
                                colrz_ev_od = colrz_b;
                                colrz_od_ev = colrz_r;
                                colrz_od_od = colrz_g;
                                cplt_ev_ev = cplt_g_on_b;
                                cplt_ev_od = cplt_b;
                                cplt_od_ev = cplt_r;
                                cplt_od_od = cplt_g_on_r;
                        } else {
                                retval = 2; /* all green is not a Bayer pattern */
                                goto finalize;
                        }
                } else {
                        retval = 3;     /* first byte neither 0x00 nor 0xff */
                        goto finalize;
                }
                png_set_IHDR(png_ptr, info_ptr, width, height,
                             8, PNG_COLOR_TYPE_RGB, PNG_INTERLACE_NONE,
                             PNG_COMPRESSION_TYPE_BASE, PNG_FILTER_TYPE_BASE);
                png_write_info(png_ptr, info_ptr);
                for (y_ev = 0, y_od = 1;
                     y_ev < height && y_od < height;
                     y_ev += 2, y_od += 2) {
                        for (x_ev = 0, x_od = 1;
                             x_ev < width && x_od < width;
                             x_ev += 2, x_od += 2) {
                                colrz_ev_ev(uncompressed + y_ev * width + x_ev,
                                            row, y_ev, x_ev);
                                colrz_ev_od(uncompressed + y_ev * width + x_od,
                                            row, y_ev, x_od);
                                if (y_ev > 0 && y_ev < height - 1) {
                                        cplt_ev_ev(uncompressed + y_ev * width + x_ev,
                                                   row, y_ev, x_ev, width);
                                        cplt_ev_od(uncompressed + y_ev * width + x_od,
                                                   row, y_ev, x_od, width);
                                }
                                raise(row, x_ev, color_raiser);
                                raise(row, x_od, color_raiser);
                        }
                        png_write_row(png_ptr, row);
                        for (x_ev = 0, x_od = 1;
                             x_ev < width && x_od < width;
                             x_ev += 2, x_od += 2) {
                                colrz_od_ev(uncompressed + y_od * width + x_ev,
                                            row, y_od, x_ev);
                                colrz_od_od(uncompressed + y_od * width + x_od,
                                            row, y_od, x_od);
                                if (y_od < height - 1) {
                                        cplt_od_ev(uncompressed + y_od * width + x_ev,
                                                   row, y_od, x_ev, width);
                                        cplt_od_od(uncompressed + y_od * width + x_od,
                                                   row, y_od, x_od, width);
                                }
                                raise(row, x_ev, color_raiser);
                                raise(row, x_od, color_raiser);
                        }
                        png_write_row(png_ptr, row);
                }
        } else if (channels == 1) {
                png_set_IHDR(png_ptr, info_ptr, width, height,
                             8, PNG_COLOR_TYPE_GRAY, PNG_INTERLACE_NONE,
                             PNG_COMPRESSION_TYPE_BASE, PNG_FILTER_TYPE_BASE);
                png_write_info(png_ptr, info_ptr);
                for (y = 0; y < height; y++) {
                        for (x = 0; x < width; x++) {
                                row[x] = uncompressed[y * width + x];
                        }
                        png_write_row(png_ptr, row);
                }
        } else {
                retval = 6;       /* wrong number of channels */
                goto finalize;
        }
	png_write_end(png_ptr, NULL);
finalize:
	if (info_ptr != NULL) png_free_data(png_ptr, info_ptr, PNG_FREE_ALL, -1);
	if (png_ptr != NULL) png_destroy_write_struct(&png_ptr, NULL);
	if (row != NULL) free(row);
	return retval;
}

int
uncompressedjpeg2png(struct mem_encode *mem_png, int width, int height,
                     int channels, double *color_raiser,
                     unsigned char *uncompressed)
{
	int retval = 0;
	png_structp png_ptr = NULL;
	png_infop info_ptr = NULL;
        int y;
        int i;

        png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
	if (png_ptr == NULL) {
		retval = 11; /* Could not allocate write struct */
		goto finalize;
	}
	info_ptr = png_create_info_struct(png_ptr);
	if (info_ptr == NULL) {
		retval = 12; /* Could not allocate info struct */
		goto finalize;
	}
	if (setjmp(png_jmpbuf(png_ptr))) {
		retval = 13; /* Error during png creation */
		goto finalize;
	}
        mem_png->buffer = NULL;
        mem_png->size = 0;
        png_set_write_fn(png_ptr, mem_png, write_png_data, flush_png);
        if (channels == 3) {
                void (*raise)(unsigned char *, int, double *) = raise_noop;
                
                for (i = 0; i < 3; i++)
                        if (fabs(color_raiser[i] - 1) > 0.00001)
                                raise = raise_color;
                for (i = 0; i < width * height; i++)
                        raise(uncompressed, i, color_raiser);
                png_set_IHDR(png_ptr, info_ptr, width, height,
                             8, PNG_COLOR_TYPE_RGB, PNG_INTERLACE_NONE,
                             PNG_COMPRESSION_TYPE_BASE, PNG_FILTER_TYPE_BASE);
        } else if (channels == 1) {
                png_set_IHDR(png_ptr, info_ptr, width, height,
                             8, PNG_COLOR_TYPE_GRAY, PNG_INTERLACE_NONE,
                             PNG_COMPRESSION_TYPE_BASE, PNG_FILTER_TYPE_BASE);
        } else {
                retval = 6;       /* wrong number of channels */
                goto finalize;
        }
        png_write_info(png_ptr, info_ptr);
        for (y = 0; y < height; y++)
                png_write_row(png_ptr, uncompressed + y * width * channels);
	png_write_end(png_ptr, NULL);
finalize:
	if (info_ptr != NULL) png_free_data(png_ptr, info_ptr, PNG_FREE_ALL, -1);
	if (png_ptr != NULL) png_destroy_write_struct(&png_ptr, NULL);
	return retval;
}

int
huffdecode(int width, int height, unsigned char *uncompressed,
           unsigned char hcode[static 4 * 511], unsigned char hlen[static 511],
           unsigned char *compressed)
{
        int i, j, m;
        int maxlen = 0;
        int code;
        struct huffval {
                int val;
                int len;
        } *htable;
        int row, column, cidx;
        uint64_t hc_mask;
        unsigned int rowhead;

        for (i = 0; i < 511; i++)
                if (hlen[i] > maxlen)
                        maxlen = hlen[i];
        if ((htable = malloc((1 << maxlen) * sizeof(struct huffval))) == NULL)
                return 31;
        /* htable may well become too big to fit into memory.  Maybe
        we should act a bit smarter and handle longer huffman codes
        differently. */
        for (i = 0; i < 511; i++) {
                if (hlen[i] > 0) {
                        code = hcode[4 * i] << 24 | hcode[4 * i + 1] << 16 |
                                hcode[4 * i + 2] << 8 | hcode[4 * i + 3];
                        code <<= maxlen - hlen[i];
                        htable[code].val = i - 255;
                        htable[code].len = hlen[i];
                        for (j = 0; j < (1 << (maxlen - hlen[i])); j++) {
                                htable[code | j].val = i - 255;
                                htable[code | j].len = hlen[i];
                        }
                }
        }
        hc_mask = 0;
        for (i = 0, m = 1; i < maxlen; i++, m <<= 1)
                hc_mask |= m;
        cidx = 0;
        for (row = 0; row < height; row++) {
                rowhead = compressed[cidx / 8] << 16;
                cidx += 8;
                rowhead |= compressed[cidx / 8] << 8;
                cidx += 8;
                rowhead |= compressed[cidx / 8];
                rowhead >>= 8 - cidx % 8;
                uncompressed[width * row] = (rowhead >> 8) & 0xff;
                uncompressed[width * row + 1] = rowhead & 0xff;
                for (column = 2; column < width; column++) {
                        div_t cidx_d;
                        int rem_bits, r;
                        uint64_t hc;
                        
                        cidx_d = div(cidx, 8);
                        rem_bits = maxlen - (8 - cidx_d.rem);
                        cidx += 8 - cidx_d.rem;
                        hc = compressed[cidx_d.quot];
                        for (i = rem_bits; i > 0; i -= 8, cidx += 8) {
                                hc <<= 8;
                                hc |= compressed[cidx / 8];
                        }
                        if ((r = rem_bits % 8) > 0) {
                                hc >>= 8 - r;
                                cidx -= 8 - r;
                        }
                        hc &= hc_mask;
                        uncompressed[width * row + column] =
                                uncompressed[width * row + column - 2] - htable[hc].val;
                        cidx -= maxlen - htable[hc].len;
                }
        }
        free(htable);
        return 0;
}

void
reverse(unsigned char *base, size_t size)
{
        int i, j;
        char tmp;

        for (i = 0, j = size - 1; i < j; i++, j--) {
                tmp = base[i];
                base[i] = base[j];
                base[j] = tmp;
        }
}

void
brighten(unsigned char *base, size_t size)
{
        size_t i;
        unsigned char min = UCHAR_MAX, max = 0, range;

        for (i = 0; i < size; i++) {
                if (base[i] < min)
                        min = base[i];
                if (base[i] > max)
                        max = base[i];
        }
        range = max - min;
        if (max < 200)
                for (i = 0; i < size; i++)
                        base[i] = (base[i] - min) * UCHAR_MAX / range;
}

struct imread_jpeg_error_mgr {
        struct jpeg_error_mgr pub;
        jmp_buf setjmp_buffer;
};

typedef struct imread_jpeg_error_mgr *imread_jpeg_error_ptr;

METHODDEF(void)
imread_jpeg_error_exit (j_common_ptr cinfo)
{
        imread_jpeg_error_ptr myerr = (imread_jpeg_error_ptr) cinfo->err;
        (*cinfo->err->output_message) (cinfo);
        longjmp(myerr->setjmp_buffer, 1);
}

int
png2mem(char *path, int start, int len, unsigned int width, unsigned int height,
        unsigned int channels, int *bayer_pattern, int compr_mode,
        unsigned char *uncompressed, unsigned char *compressed,
        struct mem_encode *mem_png,
        int reversep, int brightenp, double *color_raiser)
{
        FILE *in;
        unsigned char hlen[511], hcode[511 * 4];
        int htblsize = 511 * (1 + 4);
        int retval;

        if ((in = fopen(path, "r")) == NULL)
                return 1;
        fseek(in, start, SEEK_CUR);
        if (compr_mode == 1 || compr_mode == 2) {
                fread(hcode, sizeof(hcode[0]), 511 * 4, in);
                fread(hlen, sizeof(hlen[0]), 511, in);
                fread(compressed, sizeof(compressed[0]), len - htblsize, in);
                fclose(in);
                if ((retval = huffdecode(width, height, uncompressed, hcode,
                                         hlen, compressed)) != 0)
                        return retval;
                if (reversep)
                        reverse(uncompressed, width * height);
                if (brightenp)
                        brighten(uncompressed, width * height);
                if ((retval = uncompressed2png(mem_png, width, height, channels,
                                               bayer_pattern, color_raiser,
                                               uncompressed)) != 0)
                        return retval;
                return 0;
        } else if (compr_mode == 3) { /* JPEG */
                struct jpeg_decompress_struct cinfo;
                struct imread_jpeg_error_mgr jerr;
                unsigned char *next_line;
                int nsamples;

                cinfo.err = jpeg_std_error(&jerr.pub);
                jerr.pub.error_exit = imread_jpeg_error_exit;
                if (setjmp(jerr.setjmp_buffer)) {
                        jpeg_destroy_decompress(&cinfo);
                        return 71; /* JPEG decompression error */
                }
                jpeg_create_decompress(&cinfo);
                fread(compressed, sizeof(compressed[0]), len, in);
                fclose(in);
                jpeg_mem_src(&cinfo, compressed, len);
                jpeg_read_header(&cinfo, TRUE);
                if (cinfo.image_width * cinfo.image_height * cinfo.num_components >
                    width * height * channels) {
                        return 72; /* JPEG bigger than expected */
                        jpeg_destroy_decompress(&cinfo);
                }
                jpeg_start_decompress(&cinfo);
                next_line = uncompressed;
                while (cinfo.output_scanline < cinfo.output_height) {
                        nsamples = jpeg_read_scanlines(&cinfo, (JSAMPARRAY)&next_line, 1);
                        next_line += nsamples * cinfo.image_width * cinfo.num_components;
                }
                jpeg_finish_decompress(&cinfo);
                jpeg_destroy_decompress(&cinfo);
                if (reversep)
                        return 73; /* JPEG reversing not implemented */
                if (brightenp)
                        return 74; /* JPEG brightening not implemented */
                if ((retval = uncompressedjpeg2png(
                             mem_png, cinfo.image_width, cinfo.image_height,
                             cinfo.num_components, color_raiser,
                             uncompressed)) != 0)
                        return retval;
                return 0;
        } else if (compr_mode == 0) { /* untested */
                fread(uncompressed, sizeof(uncompressed[0]), width * height, in);
                fclose(in);
                if (reversep)
                        reverse(uncompressed, width * height);
                if (brightenp)
                        brighten(uncompressed, width * height);
                if ((retval = uncompressed2png(mem_png, width, height, channels,
                                               bayer_pattern, color_raiser,
                                               uncompressed)) != 0)
                        return retval;
                return 0;
        } else {
                fclose(in);
                return 5;
        }
}

int
ping(int n) {
        return n;
}

int
main(int argc, char *argv[])
{
        FILE *fp;
        unsigned char compressed[1390353];
        unsigned char uncompressed[1700 * 1500];
        int width = 1700, height = 1500;
        struct mem_encode mp;

        (void) argc;
        (void) argv;
        
        setvbuf(stdout, NULL, _IONBF, 0);

        png2mem("mitsa005_CCD_Front_PULNIX_13.pictures", 2247 + 17, 1390353, width, height, 3, (int[2]){0x00ff00, 0x0000ff}, 2, uncompressed, compressed, &mp, 0, 0, (double[3]){1, 1, 1});
	fp = fopen("o0.png", "wb");
	if (fp == NULL) {
		fprintf(stderr, "Could not open file %s for writing\n", "o0.png");
	}
        fwrite(mp.buffer, mp.size, sizeof(mp.buffer[0]), fp);

        png2mem("mitsa005_CCD_Front_PULNIX_13.pictures", 2247 + 17, 1390353, width, height, 3, (int[2]){0x00ff00, 0x0000ff}, 2, uncompressed, compressed, &mp, 0, 1, (double[3]){1, 2.5, 1.5});
	fp = fopen("o1.png", "wb");
	if (fp == NULL) {
		fprintf(stderr, "Could not open file %s for writing\n", "o1.png");
	}
        fwrite(mp.buffer, mp.size, sizeof(mp.buffer[0]), fp);
        if(mp.buffer)
                free(mp.buffer);
}
