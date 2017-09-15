/* PHOROS -- Photogrammetric Road Survey */
/* Copyright (C) 2017 Bert Burgemeister */

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

#ifndef IMREAD_H
#define IMREAD_H


#include <malloc.h>
#include <math.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <png.h>
#include <jpeglib.h>

/* Storage for the in-memory PNG */
struct png_store
{
        char *buffer;
        size_t size;
};

/* 
   Put a width x height image from the .pictures file at pictures_path into png.
   The data_size bytes (value of "dataSize=") of the original blob are read beginning at file position start (after PICTUREDATA_BEGIN).
   
 */
int
png2mem(char *pictures_path,    /* path to a *.pictures file */
        long long int start, /* file position after "PICTUREDATA_BEGIN" */
        int data_size,   /* value of "dataSize=" */
        unsigned int width,     /* value of "width=" */
        unsigned int height,    /* value of "height=" */
        unsigned int channels,  /* value of "channels=" */
        int *bayer_pattern,
        bool demosaic_fast,
        int compression_mode,   /* value of "compressed=" */
        struct png_store *png,  /* result */
        bool reversep,
        bool brightenp,
        double *color_raiser);

/* 
   Confirm presence of this library by returning n.
 */
int
ping(int n);


#endif  /* IMREAD_H */
