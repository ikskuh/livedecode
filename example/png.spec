
endian be
print "File Header:"

u8 magic_8bit # should be 0x89
str 5 magic # PNG\r\n
u8 magic_1a # 0x1A
u8 magic_0a # 0x0a

def tEXt 1950701684
def IHDR 1229472850
def tIME 1950960965

call chunk "IHDR"
call chunk "gAMA"
call chunk "cHRM"
call chunk "bKGD"
call chunk "tIME"
call chunk "IDAT"
call chunk "tEXt"
call chunk "tEXt"
call chunk "IEND"



.pgm chunk
  print chunk *arg[0]
  u32 chunk_length
  u32 type
  lut *type \
    (*tEXt      "text extension") \
    (*IHDR      "image header") \
    (*tIME      "creation time") \
    (1732332865 gAMA) \
    (1665684045 cHRM) \
    (1649100612 bKGD) \
    (1229209940 IDAT) \
    (1229278788 "end of file")
  move -4
  str 4 type_str
  .if *type *tEXt
    str *chunk_length text
  .elseif *type *IHDR
    u32 width
    u32 height
    u8 depth
    u8 color_type
    u8 compression
    u8 filter
    u8 interlace
  .elseif *type *tIME
    u16 year
    u8 month
    u8 day
    u8 hour
    u8 minute
    u8 second
  .else
    dump *chunk_length 
  .endif
  u32 crc
.endpgm