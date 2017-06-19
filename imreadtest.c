#include "imread.h"

int
main(int argc, char *argv[])
{
        char *in_path, *out_path;
        unsigned long width, height, channels, size, compression_mode, demosaic_fast;
        unsigned long long start;
        FILE *fp;
        int bayer_pattern[2] = {0x00ff00, 0x0000ff};
        double color_raiser[3] = {1, 1, 1};
        struct png_store mp;
        int err;

        setvbuf(stdout, NULL, _IONBF, 0);
        if (argc >= 11) {
                in_path = argv[1];
                width = strtoul(argv[2], NULL, 10);
                height = strtoul(argv[3], NULL, 10);
                channels = strtoul(argv[4], NULL, 10);
                start = strtoull(argv[5], NULL, 10);
                size = strtoul(argv[6], NULL, 10);
                compression_mode = strtoul(argv[7], NULL, 10);
                demosaic_fast = strtoul(argv[8], NULL, 10);
                sscanf(argv[9], "%lf, %lf, %lf",
                       &color_raiser[0], &color_raiser[1], &color_raiser[2]);
                out_path = argv[10];
                err = png2mem(in_path, start, size, width, height, channels,
                              bayer_pattern, demosaic_fast, compression_mode,
                              &mp, false, false, color_raiser);
                fp = fopen(out_path, "wb");
                if (fp == NULL) {
                        fprintf(stderr, "Could not open file %s for writing\n", out_path);
                        return 2;
                }
                fwrite(mp.buffer, mp.size, sizeof(mp.buffer[0]), fp);
                printf("status=%d\n", err);
                printf("in_path=%s width=%ld height=%ld channels=%ld "
                       "start=%lld size=%ld compression_mode=%ld "
                       "demosaic_fast=%ld color_raiser=%.1lf,%.1lf,%.1lf "
                       "out_path=%s\n",
                       in_path, width, height, channels, start, size,
                       compression_mode, demosaic_fast,
                       color_raiser[0], color_raiser[1], color_raiser[2],
                       out_path);
                if(mp.buffer)
                        free(mp.buffer);
                return 0;
        }
        if (argc == 2) {
                printf("ping = %d\n", ping(strtoul(argv[1], NULL, 10)));
                return 0;
        }
        printf("Usage: %s in_path width height channels start size "
               "compression_mode demosaic_fast color_raiser out_path\n",
               argv[0]);
        return 1;
}

/* ./imreadtest mitsa005_CCD_Front_PULNIX_13.pictures 1700 1500 3 2264 1392908 2 1 1,1,1 m.png */
/* ./imreadtest phoros_20110405_002_CCDHECK_01_PULNIX_11.pictures 1920 1080 1 1363 1384533 2 1 1,1,1 p.png */
/* ./imreadtest 15294_IRIS3_20160519_000_CCD_RETRO_ZEB_1.pictures 1920 1080 4 1210 598189 3 1 1,1,1 j.png */
/* ./imreadtest 15294_IRIS3_20160609_004_CCD_Front_1.pictures 1280 960 3 2009 723324 2 1 1,1,1 b.png */
/* ./imreadtest 15294_IRIS3_20160609_004_CCD_Front_1.pictures 1280 960 3 2125833547 682281 2 1 1,1,1 b.png */
/* ./imreadtest 15294_IRIS3_20160609_004_CCD_Front_1.pictures 1280 960 3 2185929330 682393 2 1 1,1,1 b.png */
