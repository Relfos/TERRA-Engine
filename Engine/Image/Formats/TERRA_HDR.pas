static int hdr_test(stbi *s)
{
   const char *signature = "#?RADIANCE\n";
   int i;
   for (i=0; signature[i]; ++i)
      if (get8(s) != signature[i])
         return 0;
   return 1;
}

static int stbi_hdr_test(stbi* s)
{
   int r = hdr_test(s);
   stbi_rewind(s);
   return r;
}

#define HDR_BUFLEN  1024
static char *hdr_gettoken(stbi *z, char *buffer)
{
   int len=0;
   char c = '\0';

   c = (char) get8(z);

   while (!at_eof(z) && c != '\n') {
      buffer[len++] = c;
      if (len == HDR_BUFLEN-1) {
         // flush to end of line
         while (!at_eof(z) && get8(z) != '\n')
            ;
         break;
      }
      c = (char) get8(z);
   }

   buffer[len] = 0;
   return buffer;
}

static void hdr_convert(float *output, stbi_uc *input, int req_comp)
{
   if ( input[3] != 0 ) {
      float f1;
      // Exponent
      f1 = (float) ldexp(1.0f, input[3] - (int)(128 + 8));
      if (req_comp <= 2)
         output[0] = (input[0] + input[1] + input[2]) * f1 / 3;
      else {
         output[0] = input[0] * f1;
         output[1] = input[1] * f1;
         output[2] = input[2] * f1;
      }
      if (req_comp == 2) output[1] = 1;
      if (req_comp == 4) output[3] = 1;
   } else {
      switch (req_comp) {
         case 4: output[3] = 1; /* fallthrough */
         case 3: output[0] = output[1] = output[2] = 0;
                 break;
         case 2: output[1] = 1; /* fallthrough */
         case 1: output[0] = 0;
                 break;
      }
   }
}

static float *hdr_load(stbi *s, int *x, int *y, int *comp, int req_comp)
{
   char buffer[HDR_BUFLEN];
   char *token;
   int valid = 0;
   int width, height;
   stbi_uc *scanline;
   float *hdr_data;
   int len;
   unsigned char count, value;
   int i, j, k, c1,c2, z;


   // Check identifier
   if (strcmp(hdr_gettoken(s,buffer), "#?RADIANCE") != 0)
      return epf("not HDR", "Corrupt HDR image");
   
   // Parse header
   for(;;) {
      token = hdr_gettoken(s,buffer);
      if (token[0] == 0) break;
      if (strcmp(token, "FORMAT=32-bit_rle_rgbe") == 0) valid = 1;
   }

   if (!valid)    return epf("unsupported format", "Unsupported HDR format");

   // Parse width and height
   // can't use sscanf() if we're not using stdio!
   token = hdr_gettoken(s,buffer);
   if (strncmp(token, "-Y ", 3))  return epf("unsupported data layout", "Unsupported HDR format");
   token += 3;
   height = strtol(token, &token, 10);
   while (*token == ' ') ++token;
   if (strncmp(token, "+X ", 3))  return epf("unsupported data layout", "Unsupported HDR format");
   token += 3;
   width = strtol(token, NULL, 10);

   *x = width;
   *y = height;

   *comp = 3;
   if (req_comp == 0) req_comp = 3;

   // Read data
   hdr_data = (float *) malloc(height * width * req_comp * sizeof(float));

   // Load image data
   // image data is stored as some number of sca
   if ( width < 8 || width >= 32768) {
      // Read flat data
      for (j=0; j < height; ++j) {
         for (i=0; i < width; ++i) {
            stbi_uc rgbe[4];
           main_decode_loop:
            getn(s, rgbe, 4);
            hdr_convert(hdr_data + j * width * req_comp + i * req_comp, rgbe, req_comp);
         }
      }
   } else {
      // Read RLE-encoded data
      scanline = NULL;

      for (j = 0; j < height; ++j) {
         c1 = get8(s);
         c2 = get8(s);
         len = get8(s);
         if (c1 != 2 || c2 != 2 || (len & 0x80)) {
            // not run-length encoded, so we have to actually use THIS data as a decoded
            // pixel (note this can't be a valid pixel--one of RGB must be >= 128)
            uint8 rgbe[4];
            rgbe[0] = (uint8) c1;
            rgbe[1] = (uint8) c2;
            rgbe[2] = (uint8) len;
            rgbe[3] = (uint8) get8u(s);
            hdr_convert(hdr_data, rgbe, req_comp);
            i = 1;
            j = 0;
            free(scanline);
            goto main_decode_loop; // yes, this makes no sense
         }
         len <<= 8;
         len |= get8(s);
         if (len != width) { free(hdr_data); free(scanline); return epf("invalid decoded scanline length", "corrupt HDR"); }
         if (scanline == NULL) scanline = (stbi_uc *) malloc(width * 4);
            
         for (k = 0; k < 4; ++k) {
            i = 0;
            while (i < width) {
               count = get8u(s);
               if (count > 128) {
                  // Run
                  value = get8u(s);
                  count -= 128;
                  for (z = 0; z < count; ++z)
                     scanline[i++ * 4 + k] = value;
               } else {
                  // Dump
                  for (z = 0; z < count; ++z)
                     scanline[i++ * 4 + k] = get8u(s);
               }
            }
         }
         for (i=0; i < width; ++i)
            hdr_convert(hdr_data+(j*width + i)*req_comp, scanline + i*4, req_comp);
      }
      free(scanline);
   }

   return hdr_data;
}

static float *stbi_hdr_load(stbi *s, int *x, int *y, int *comp, int req_comp)
{
   return hdr_load(s,x,y,comp,req_comp);
}

static int stbi_hdr_info(stbi *s, int *x, int *y, int *comp)
{
   char buffer[HDR_BUFLEN];
   char *token;
   int valid = 0;

   if (strcmp(hdr_gettoken(s,buffer), "#?RADIANCE") != 0) {
       stbi_rewind( s );
       return 0;
   }

   for(;;) {
      token = hdr_gettoken(s,buffer);
      if (token[0] == 0) break;
      if (strcmp(token, "FORMAT=32-bit_rle_rgbe") == 0) valid = 1;
   }

   if (!valid) {
       stbi_rewind( s );
       return 0;
   }
   token = hdr_gettoken(s,buffer);
   if (strncmp(token, "-Y ", 3)) {
       stbi_rewind( s );
       return 0;
   }
   token += 3;
   *y = strtol(token, &token, 10);
   while (*token == ' ') ++token;
   if (strncmp(token, "+X ", 3)) {
       stbi_rewind( s );
       return 0;
   }
   token += 3;
   *x = strtol(token, NULL, 10);
   *comp = 3;
   return 1;
}
