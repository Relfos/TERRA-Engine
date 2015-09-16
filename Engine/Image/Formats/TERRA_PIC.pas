// *************************************************************************************************
// Softimage PIC loader
// by Tom Seddon
//
// See http://softimage.wiki.softimage.com/index.php/INFO:_PIC_file_format
// See http://ozviz.wasp.uwa.edu.au/~pbourke/dataformats/softimagepic/

static int pic_is4(stbi *s,const char *str)
{
   int i;
   for (i=0; i<4; ++i)
      if (get8(s) != (stbi_uc)str[i])
         return 0;

   return 1;
}

static int pic_test(stbi *s)
{
   int i;

   if (!pic_is4(s,"\x53\x80\xF6\x34"))
      return 0;

   for(i=0;i<84;++i)
      get8(s);

   if (!pic_is4(s,"PICT"))
      return 0;

   return 1;
}

typedef struct
{
   stbi_uc size,type,channel;
} pic_packet_t;

static stbi_uc *pic_readval(stbi *s, int channel, stbi_uc *dest)
{
   int mask=0x80, i;

   for (i=0; i<4; ++i, mask>>=1) {
      if (channel & mask) {
         if (at_eof(s)) return epuc("bad file","PIC file too short");
         dest[i]=get8u(s);
      }
   }

   return dest;
}

static void pic_copyval(int channel,stbi_uc *dest,const stbi_uc *src)
{
   int mask=0x80,i;

   for (i=0;i<4; ++i, mask>>=1)
      if (channel&mask)
         dest[i]=src[i];
}

static stbi_uc *pic_load2(stbi *s,int width,int height,int *comp, stbi_uc *result)
{
   int act_comp=0,num_packets=0,y,chained;
   pic_packet_t packets[10];

   // this will (should...) cater for even some bizarre stuff like having data
    // for the same channel in multiple packets.
   do {
      pic_packet_t *packet;

      if (num_packets==sizeof(packets)/sizeof(packets[0]))
         return epuc("bad format","too many packets");

      packet = &packets[num_packets++];

      chained = get8(s);
      packet->size    = get8u(s);
      packet->type    = get8u(s);
      packet->channel = get8u(s);

      act_comp |= packet->channel;

      if (at_eof(s))          return epuc("bad file","file too short (reading packets)");
      if (packet->size != 8)  return epuc("bad format","packet isn't 8bpp");
   } while (chained);

   *comp = (act_comp & 0x10 ? 4 : 3); // has alpha channel?

   for(y=0; y<height; ++y) {
      int packet_idx;

      for(packet_idx=0; packet_idx < num_packets; ++packet_idx) {
         pic_packet_t *packet = &packets[packet_idx];
         stbi_uc *dest = result+y*width*4;

         switch (packet->type) {
            default:
               return epuc("bad format","packet has bad compression type");

            case 0: {//uncompressed
               int x;

               for(x=0;x<width;++x, dest+=4)
                  if (!pic_readval(s,packet->channel,dest))
                     return 0;
               break;
            }

            case 1://Pure RLE
               {
                  int left=width, i;

                  while (left>0) {
                     stbi_uc count,value[4];

                     count=get8u(s);
                     if (at_eof(s))   return epuc("bad file","file too short (pure read count)");

                     if (count > left)
                        count = (uint8) left;

                     if (!pic_readval(s,packet->channel,value))  return 0;

                     for(i=0; i<count; ++i,dest+=4)
                        pic_copyval(packet->channel,dest,value);
                     left -= count;
                  }
               }
               break;

            case 2: {//Mixed RLE
               int left=width;
               while (left>0) {
                  int count = get8(s), i;
                  if (at_eof(s))  return epuc("bad file","file too short (mixed read count)");

                  if (count >= 128) { // Repeated
                     stbi_uc value[4];
                     int i;

                     if (count==128)
                        count = get16(s);
                     else
                        count -= 127;
                     if (count > left)
                        return epuc("bad file","scanline overrun");

                     if (!pic_readval(s,packet->channel,value))
                        return 0;

                     for(i=0;i<count;++i, dest += 4)
                        pic_copyval(packet->channel,dest,value);
                  } else { // Raw
                     ++count;
                     if (count>left) return epuc("bad file","scanline overrun");

                     for(i=0;i<count;++i, dest+=4)
                        if (!pic_readval(s,packet->channel,dest))
                           return 0;
                  }
                  left-=count;
               }
               break;
            }
         }
      }
   }

   return result;
}

static stbi_uc *pic_load(stbi *s,int *px,int *py,int *comp,int req_comp)
{
   stbi_uc *result;
   int i, x,y;

   for (i=0; i<92; ++i)
      get8(s);

   x = get16(s);
   y = get16(s);
   if (at_eof(s))  return epuc("bad file","file too short (pic header)");
   if ((1 << 28) / x < y) return epuc("too large", "Image too large to decode");

   get32(s); //skip `ratio'
   get16(s); //skip `fields'
   get16(s); //skip `pad'

   // intermediate buffer is RGBA
   result = (stbi_uc *) malloc(x*y*4);
   memset(result, 0xff, x*y*4);

   if (!pic_load2(s,x,y,comp, result)) {
      free(result);
      result=0;
   }
   *px = x;
   *py = y;
   if (req_comp == 0) req_comp = *comp;
   result=convert_format(result,4,req_comp,x,y);

   return result;
}

static int stbi_pic_test(stbi *s)
{
   int r = pic_test(s);
   stbi_rewind(s);
   return r;
}

static stbi_uc *stbi_pic_load(stbi *s, int *x, int *y, int *comp, int req_comp)
{
   return pic_load(s,x,y,comp,req_comp);
}
