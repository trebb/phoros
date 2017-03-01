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

#include "imread.h"

enum color {RED, GREEN, BLUE};

/* 
   Set one particular color cell to the value of an adjacent pixel/to
   the average of the values in the surrounding pixels found in rawp
 */
static void
cplt_horiz_fast(unsigned char *rawp, png_bytep cell, int color, int width)
{
        (void) width;
        cell[color] = rawp[1];
}

static void
cplt_horiz_slow(unsigned char *rawp, png_bytep cell, int color, int width)
{
        (void) width;
        cell[color] = (rawp[-1] + rawp[1]) / 2;
}

static void
cplt_vert_fast(unsigned char *rawp, png_bytep cell, int color, int width)
{
        cell[color] = rawp[width];
}

static void
cplt_vert_slow(unsigned char *rawp, png_bytep cell, int color, int width)
{
        cell[color] = (rawp[-width] + rawp[width]) / 2;
}

static void
cplt_squ_fast(unsigned char *rawp, png_bytep cell, int color, int width)
{
        (void) width;
        cell[color] = rawp[1];
}

static void
cplt_squ_slow(unsigned char *rawp, png_bytep cell, int color, int width)
{
        cell[color] = (rawp[-1] + rawp[1] + rawp[-width] + rawp[width]) / 4;
}

static void
cplt_diag_fast(unsigned char *rawp, png_bytep cell, int color, int width)
{
        cell[color] = rawp[width + 1];
}

static void
cplt_diag_slow(unsigned char *rawp, png_bytep cell, int color, int width)
{
        cell[color] = (rawp[-width - 1] + rawp[-width + 1] +
                      rawp[width - 1] + rawp[width + 1]) / 4;
}

typedef void (*GeometricCompleter)(unsigned char *, png_bytep, int, int);

GeometricCompleter cplt_horiz, cplt_vert, cplt_squ, cplt_diag;

/* 
    Add missing colors to a green cell in a red and green row
 */
static void
cplt_g_on_r(unsigned char *rawp, png_bytep cell, int width)
{
        cplt_horiz(rawp, cell, RED, width);
        cplt_vert(rawp, cell, BLUE, width);
}

/* 
    Add missing colors to a green cell in a blue and green row
 */
static void
cplt_g_on_b(unsigned char *rawp, png_bytep cell, int width)
{
        cplt_horiz(rawp, cell, BLUE, width);
        cplt_vert(rawp, cell, RED, width);
}

/* 
    Add missing colors to a red cell
 */
static void
cplt_r(unsigned char *rawp, png_bytep cell, int width)
{
        cplt_squ(rawp, cell, GREEN, width);
        cplt_diag(rawp, cell, BLUE, width);
}

/* 
    Add missing colors to a blue cell
 */
static void
cplt_b(unsigned char *rawp, png_bytep cell, int width)
{
        cplt_squ(rawp, cell, GREEN, width);
        cplt_diag(rawp, cell, RED, width);
}

typedef void (*Completer)(unsigned char *, png_bytep, int);

/*
   Set one particular color of cell the value found in rawp
 */
static void
colrz_r(unsigned char *rawp, png_bytep cell)
{
        cell[RED] = rawp[0];
}
        
static void
colrz_g(unsigned char *rawp, png_bytep cell)
{
        cell[GREEN] = rawp[0];
}
        
static void
colrz_b(unsigned char *rawp, png_bytep cell)
{
        cell[BLUE] = rawp[0];
}

typedef void (*Colorizer)(unsigned char *, png_bytep);

static void
raise_color(png_bytep cell, double *colr_raisr)
{
        int i, val;
        
        for (i = 0; i < 3; i++) {
                val = round(cell[i] * colr_raisr[i]);
                if (val > 255)
                        val = 255;
                cell[i] = val;
        }
}
        
static void
raise_noop(png_bytep cell, double *colr_raisr)
{
        (void) cell;
        (void) colr_raisr;
}

typedef void (*ColorRaiser)(unsigned char *, double *);

static void
write_png_data(png_structp png_ptr, png_bytep data, png_size_t length)
{
        struct png_store *p = (struct png_store *)png_get_io_ptr(png_ptr);
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

static void
flush_png(png_structp png_ptr)
{
        (void) png_ptr;
}

static int
raw_img2png(struct png_store *png, int width, int height, int channels,
            int *bayerpat, double *color_raiser, unsigned char *raw_img,
            bool demosaic_fast)
{
	int retval = 0;
	png_structp png_ptr = NULL;
	png_infop info_ptr = NULL;
	png_bytep ev_row = NULL, od_row = NULL;
        Colorizer colrz_ev_ev, colrz_ev_od, colrz_od_ev, colrz_od_od;
        Completer cplt_ev_ev, cplt_ev_od, cplt_od_ev, cplt_od_od;
        int i, x, y, x_od, y_od;

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
        png->buffer = NULL;
        png->size = 0;
        png_set_write_fn(png_ptr, png, write_png_data, flush_png);
	if (((ev_row = malloc(channels * width * sizeof(png_byte))) == NULL) ||
            ((od_row = malloc(channels * width * sizeof(png_byte))) == NULL)) {
                retval = 21;    /* Error while writing PNG row */
                goto finalize;
        }
        if (channels == 3) {
                ColorRaiser raise = raise_noop;

                if (demosaic_fast) {
                        cplt_horiz = cplt_horiz_fast;
                        cplt_vert = cplt_vert_fast;
                        cplt_squ = cplt_squ_fast;
                        cplt_diag = cplt_diag_fast;
                } else {
                        cplt_horiz = cplt_horiz_slow;
                        cplt_vert = cplt_vert_slow;
                        cplt_squ = cplt_squ_slow;
                        cplt_diag = cplt_diag_slow;
                }
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
                for (y_od = 1; y_od <= height; y_od += 2) {
                        /* neglecting bottom row if height is even */
                        int y_ev = y_od - 1;
                        unsigned char *ev_1stcol = raw_img + y_ev * width;
                        unsigned char *od_1stcol = raw_img + y_od * width;
                        
                        for (x_od = 1; x_od < width; x_od += 2) {
                                int x_ev = x_od - 1;
                                int ev_xpos = 3 * x_ev;
                                int od_xpos = 3 * x_od;

                                colrz_ev_ev(ev_1stcol + x_ev, ev_row + ev_xpos);
                                colrz_ev_od(ev_1stcol + x_od, ev_row + od_xpos);
                                colrz_od_ev(od_1stcol + x_ev, od_row + ev_xpos);
                                colrz_od_od(od_1stcol + x_od, od_row + od_xpos);
                                if (y_od > 1 && y_od < height - 1) {
                                        /* neglecting bottom row if height is odd */
                                        cplt_ev_ev(ev_1stcol + x_ev,
                                                   ev_row + ev_xpos, width);
                                        cplt_ev_od(ev_1stcol + x_od,
                                                   ev_row + od_xpos, width);
                                        cplt_od_ev(od_1stcol + x_ev,
                                                   od_row + ev_xpos, width);
                                        cplt_od_od(od_1stcol + x_od,
                                                   od_row + od_xpos, width);
                                }
                                raise(ev_row + ev_xpos, color_raiser);
                                raise(ev_row + od_xpos, color_raiser);
                                raise(od_row + ev_xpos, color_raiser);
                                raise(od_row + od_xpos, color_raiser);
                        }
                        png_write_row(png_ptr, ev_row);
                        png_write_row(png_ptr, od_row);
                }
        } else if (channels == 1) {
                png_set_IHDR(png_ptr, info_ptr, width, height,
                             8, PNG_COLOR_TYPE_GRAY, PNG_INTERLACE_NONE,
                             PNG_COMPRESSION_TYPE_BASE, PNG_FILTER_TYPE_BASE);
                png_write_info(png_ptr, info_ptr);
                for (y = 0; y < height; y++) {
                        for (x = 0; x < width; x++) {
                                ev_row[x] = raw_img[y * width + x];
                        }
                        png_write_row(png_ptr, ev_row);
                }
        } else {
                retval = 6;       /* wrong number of channels */
                goto finalize;
        }
	png_write_end(png_ptr, NULL);
finalize:
	if (info_ptr != NULL) png_free_data(png_ptr, info_ptr, PNG_FREE_ALL, -1);
	if (png_ptr != NULL) png_destroy_write_struct(&png_ptr, NULL);
	if (ev_row != NULL) free(ev_row);
	if (od_row != NULL) free(od_row);
	return retval;
}

static int
raw_jpeg2png(struct png_store *png, int width, int height,
                     int channels, double *color_raiser,
                     unsigned char *raw_jpeg)
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
        png->buffer = NULL;
        png->size = 0;
        png_set_write_fn(png_ptr, png, write_png_data, flush_png);
        if (channels == 3) {
                ColorRaiser raise = raise_noop;
                
                for (i = 0; i < 3; i++)
                        if (fabs(color_raiser[i] - 1) > 0.00001)
                                raise = raise_color;
                for (i = 0; i < 3 * width * height; i += 3)
                        raise(raw_jpeg + i, color_raiser);
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
                png_write_row(png_ptr, raw_jpeg + y * width * channels);
	png_write_end(png_ptr, NULL);
finalize:
	if (info_ptr != NULL) png_free_data(png_ptr, info_ptr, PNG_FREE_ALL, -1);
	if (png_ptr != NULL) png_destroy_write_struct(&png_ptr, NULL);
	return retval;
}

/* 
   Huffman decoder.  The size of compressed must exceed its contents
   by 8 bytes.
 */
static int
huffdecode(int width, int height, unsigned char *uncompressed,
           uint8_t hcode[static 4 * 511], uint8_t hlen[static 511],
           unsigned char *compressed, size_t compressed_size)
{
        int i, i4, j, maxlen = 0, code, retval = 0, row, column;
        uint64_t hc_mask;
        uint32_t rowhead;
        size_t cidx = 0, max_cidx = (compressed_size + 8) * 8;
        struct huffval {
                int16_t val;
                uint8_t len;
        } *htable;

        for (i = 0; i < 511; i++)
                if (hlen[i] > maxlen)
                        maxlen = hlen[i];
        if ((htable = malloc((1 << maxlen) * sizeof(struct huffval))) == NULL)
                return 31;  /* Couldn't allocate memory for huffman table */
        /* htable may well become too big to fit into memory.  Maybe
        we should act a bit smarter and handle longer huffman codes
        differently. */
        for (i = 0, i4 = 0; i < 511; i++, i4 += 4) {
                if (hlen[i] > 0) {
                        code = hcode[i4] << 24 | hcode[i4 + 1] << 16 |
                                hcode[i4 + 2] << 8 | hcode[i4 + 3];
                        code <<= maxlen - hlen[i];
                        /* one entry for each number of width maxlen
                        that starts with code: */
                        for (j = 0; j < (1 << (maxlen - hlen[i])); j++) {
                                htable[code | j].val = i - 255;
                                htable[code | j].len = hlen[i];
                        }
                }
        }
        hc_mask = (1 << maxlen) - 1;
        for (row = 0; row < height; row++) {
                /* Columns one and two of each row aren't compressed */
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
                        int bits_to_read;
                        uint64_t hc;
                        
                        cidx_d = div(cidx, 8);
                        cidx = cidx_d.quot * 8;
                        bits_to_read = maxlen + cidx_d.rem;
                        hc = 0;
                        for (i = bits_to_read; i > 0; i -= 8) {
                                hc <<= 8;
                                hc |= compressed[cidx / 8];
                                cidx += 8;
                        }
                        hc >>= -i; /* i <= 0 */
                        if ((cidx += i) > max_cidx) {
                                retval = 32; /* Huffman decoder out of step */
                                goto cleanup;
                        }
                        hc &= hc_mask;
                        uncompressed[width * row + column] =
                                uncompressed[width * row + column - 2] - htable[hc].val;
                        cidx -= maxlen - htable[hc].len;
                }
        }
cleanup:
        free(htable);
        return retval;
}

static void
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

static void
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
        unsigned int channels, int *bayer_pattern, bool demosaic_fast, int compr_mode,
        struct png_store *png, bool reversep, bool brightenp, double *color_raiser)
{
        FILE *in;
        unsigned char hlen[511], hcode[511 * 4];
        int htblsize = 511 * (1 + 4);
        int retval = 0xFFFF;    /* will change */

        if ((in = fopen(path, "r")) == NULL)
                return 1;       /* File not found */
        fseek(in, start, SEEK_CUR);
        if (compr_mode == 1 || compr_mode == 2) {
                unsigned char *in_buf, *raw_img;
                
                fread(hcode, sizeof(hcode[0]), 511 * 4, in);
                fread(hlen, sizeof(hlen[0]), 511, in);
                if ((in_buf = malloc(len + 8)) == NULL)
                        return 76; /* Couldn't allocate buffer for image data input */
                fread(in_buf, sizeof(in_buf[0]), len - htblsize, in);
                fclose(in);
                if ((raw_img = malloc(width * height)) == NULL) {
                        free(in_buf);
                        return 75; /* Couldn't allocate memory for uncompressed image */
                }
                if ((retval = huffdecode(width, height, raw_img, hcode,
                                         hlen, in_buf, len - htblsize)) == 0) {
                        if (reversep)
                                reverse(raw_img, width * height);
                        if (brightenp)
                                brighten(raw_img, width * height);
                        retval = raw_img2png(png, width, height, channels,
                                             bayer_pattern, color_raiser, raw_img,
                                             demosaic_fast);
                }
                free(raw_img);
                free(in_buf);
                return retval;
        }
        if (compr_mode == 3) { /* JPEG */
                struct jpeg_decompress_struct cinfo;
                struct imread_jpeg_error_mgr jerr;
                unsigned char *in_buf, *raw_img, *next_line;
                int nsamples;

                if (reversep)
                        return 73; /* JPEG reversing not implemented */
                if (brightenp)
                        return 74; /* JPEG brightening not implemented */
                cinfo.err = jpeg_std_error(&jerr.pub);
                jerr.pub.error_exit = imread_jpeg_error_exit;
                if (setjmp(jerr.setjmp_buffer)) {
                        jpeg_destroy_decompress(&cinfo);
                        return 71; /* JPEG decompression error */
                }
                jpeg_create_decompress(&cinfo);
                if ((in_buf = malloc(len)) == NULL)
                        return 76;
                fread(in_buf, sizeof(in_buf[0]), len, in);
                fclose(in);
                jpeg_mem_src(&cinfo, in_buf, len);
                jpeg_read_header(&cinfo, TRUE);
                if (cinfo.image_width * cinfo.image_height * cinfo.num_components >
                    width * height * channels) {
                        jpeg_destroy_decompress(&cinfo);
                        return 72; /* JPEG bigger than expected */
                }
                jpeg_start_decompress(&cinfo);
                if ((raw_img = malloc(width * height * channels)) == NULL) {
                        free(in_buf);
                        return 75;
                }
                next_line = raw_img;
                while (cinfo.output_scanline < cinfo.output_height) {
                        nsamples = jpeg_read_scanlines(&cinfo, (JSAMPARRAY)&next_line, 1);
                        next_line += nsamples * cinfo.image_width * cinfo.num_components;
                }
                jpeg_finish_decompress(&cinfo);
                jpeg_destroy_decompress(&cinfo);
                retval = raw_jpeg2png(png, cinfo.image_width, cinfo.image_height,
                                      cinfo.num_components, color_raiser,
                                      raw_img);
                free(raw_img);
                free(in_buf);
                return retval;
        }
        if (compr_mode == 0) { /* untested */
                unsigned char *raw_img;
                
                if ((raw_img = malloc(width * height)) == NULL)
                        return 75;
                fread(raw_img, sizeof(raw_img[0]), width * height, in);
                fclose(in);
                if (reversep)
                        reverse(raw_img, width * height);
                if (brightenp)
                        brighten(raw_img, width * height);
                retval = raw_img2png(png, width, height, channels,
                                     bayer_pattern, color_raiser, raw_img,
                                     demosaic_fast);
                free(raw_img);
                return retval;
        }
        fclose(in);
        return 5;       /* Unknown compression mode */
}

int
ping(int n) {
        return n;
}
