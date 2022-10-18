
endian be
print header

u8 magic_8bit # should be 0x89
str 5 magic # PNG\r\n
u8 magic_1a # 0x1A
u8 magic_0a # 0x0a

.def tEXt 1950701684
.def IHDR 1229472850
.def tIME 1950960965

pgm chunk
! print chunk *arg[0]
! u32 chunk_length
! u32 type 
! move -4
! str 4 type_str
! .if *type *tEXt
!   str *chunk_length text
! .else
!   .if *type *IHDR
!     u32 width
!     u32 height
!     u8 depth
!     u8 color_type
!     u8 compression
!     u8 filter
!     u8 interlace
!   .else
!     .if *type *tIME
!       u16 year
!       u8 month
!       u8 day
!       u8 hour
!       u8 minute
!       u8 second
!     .else
!       dump *chunk_length 
!     .endif
!   .endif
! .endif
! u32 crc

replay chunk "IHDR"
replay chunk "gAMA"
replay chunk "cHRM"
replay chunk "bKGD"
replay chunk "tIME"
replay chunk "IDAT"
replay chunk "tEXt"
replay chunk "tEXt"
replay chunk "IEND"

