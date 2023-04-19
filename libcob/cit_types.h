/*
 * Copyright (C) 2008-2019 Cobol-IT
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, 51 Franklin Street, Fifth Floor
 * Boston, MA 02110-1301 USA
 */

#ifndef CIT_TYPES_H
#define CIT_TYPES_H
/* Some usefull types */

#ifndef _WIN32
#if 1
#define COB_PTR_64BITS  1
#endif
#else
#ifdef _WIN64
#define COB_PTR_64BITS  1
#endif
#endif
#define cit_uint8    unsigned char
#define cit_int8     signed char
#define cit_uint16   unsigned short
#define cit_int16    signed short
#define cit_int32    signed int
#define cit_uint32   unsigned int
#define cit_uint64   unsigned long long

/*CIT_BEGIN_ENTERPRISE*/
#if defined(COB_EBCDIC_MACHINE) 
#define CIT_EBCDIC_CHARSET 1
#define CIT_EBCDIC_CHARSET_DEFAULT 1
#endif

#if 0
#define CIT_DUAL_CHARSET 1
#define CIT_EBCDIC_CHARSET 1
#endif

#if 0
#define CIT_EBCDIC_CHARSET 1
#define CIT_EBCDIC_CHARSET_DEFAULT 1
#endif


/* EBCDIC CHARACTER SET */
#define EBCDIC_CHAR_0         ((unsigned char)0xf0)
#define EBCDIC_CHAR_1         ((unsigned char)0xf1)
#define EBCDIC_CHAR_2         ((unsigned char)0xf2)
#define EBCDIC_CHAR_3         ((unsigned char)0xf3)
#define EBCDIC_CHAR_4         ((unsigned char)0xf4)
#define EBCDIC_CHAR_5         ((unsigned char)0xf5)
#define EBCDIC_CHAR_6         ((unsigned char)0xf6)
#define EBCDIC_CHAR_7         ((unsigned char)0xf7)
#define EBCDIC_CHAR_8         ((unsigned char)0xf8)
#define EBCDIC_CHAR_9         ((unsigned char)0xf9)
#define EBCDIC_CHAR_SP        ((unsigned char)0x40)
#define EBCDIC_CHAR_TAB       ((unsigned char)0x05)
#define EBCDIC_CHAR_LF        ((unsigned char)0x25)
#define EBCDIC_CHAR_VT        ((unsigned char)0x0B)
#define EBCDIC_CHAR_FF        ((unsigned char)0x0C)
#define EBCDIC_CHAR_CR        ((unsigned char)0x0D)
#define EBCDIC_CHAR_PLUS      ((unsigned char)0x4E)
#define EBCDIC_CHAR_MINUS     ((unsigned char)0x60)
#define EBCDIC_CHAR_DOT       ((unsigned char)0x4B)
#define EBCDIC_CHAR_COMMA     ((unsigned char)0x6B)
#define EBCDIC_CHAR_DOLLAR    ((unsigned char)0x5B) /* $ */
#define EBCDIC_CHAR_AMP       ((unsigned char)0x50) /* & */
#define EBCDIC_CHAR_QUESTION  ((unsigned char)0x6F) /* ? */
#define EBCDIC_CHAR_STAR      ((unsigned char)0x5C) /* * */
#define EBCDIC_CHAR_SLASH     ((unsigned char)0x61) /* / */
#define EBCDIC_CHAR_A         ((unsigned char)0xC1) /* A */
#define EBCDIC_CHAR_B         ((unsigned char)0xC2) /* B */
#define EBCDIC_CHAR_C         ((unsigned char)0xC3) /* C */
#define EBCDIC_CHAR_D         ((unsigned char)0xC4) /* D */
#define EBCDIC_CHAR_E         ((unsigned char)0xC5) /* E */
#define EBCDIC_CHAR_F         ((unsigned char)0xC6) /* F */
#define EBCDIC_CHAR_G         ((unsigned char)0xC7) /* G */
#define EBCDIC_CHAR_H         ((unsigned char)0xC8) /* H */
#define EBCDIC_CHAR_I         ((unsigned char)0xC9) /* I */
#define EBCDIC_CHAR_J         ((unsigned char)0xD1) /* J */
#define EBCDIC_CHAR_K         ((unsigned char)0xD2) /* K */
#define EBCDIC_CHAR_L         ((unsigned char)0xD3) /* L */
#define EBCDIC_CHAR_M         ((unsigned char)0xD4) /* M */
#define EBCDIC_CHAR_N         ((unsigned char)0xD5) /* N */
#define EBCDIC_CHAR_O         ((unsigned char)0xD6) /* O */
#define EBCDIC_CHAR_P         ((unsigned char)0xD7) /* P */
#define EBCDIC_CHAR_Q         ((unsigned char)0xD8) /* Q */
#define EBCDIC_CHAR_R         ((unsigned char)0xD9) /* R */
#define EBCDIC_CHAR_S         ((unsigned char)0xE2) /* S */
#define EBCDIC_CHAR_T         ((unsigned char)0xE3) /* T */
#define EBCDIC_CHAR_U         ((unsigned char)0xE4) /* U */
#define EBCDIC_CHAR_V         ((unsigned char)0xE5) /* V */
#define EBCDIC_CHAR_W         ((unsigned char)0xE6) /* W */
#define EBCDIC_CHAR_X         ((unsigned char)0xE7) /* X */
#define EBCDIC_CHAR_Y         ((unsigned char)0xE8) /* Y */
#define EBCDIC_CHAR_Z         ((unsigned char)0xE9) /* Z */
#define EBCDIC_CHAR_OPENTAB   ((unsigned char)0x4A) /* [ */
#define EBCDIC_CHAR_BACKSLASH ((unsigned char)0xE0) /* \ */
#define EBCDIC_CHAR_CLOSETAB  ((unsigned char)0x5A) /* ] */
#define EBCDIC_CHAR_CARET     ((unsigned char)0x5F) /* ^ */
#define EBCDIC_CHAR_UNDER     ((unsigned char)0x6D) /* _ */
#define EBCDIC_CHAR_BACKCOTE  ((unsigned char)0x79) /* ` */
#define EBCDIC_CHAR_OPENAC    ((unsigned char)0xC0) /* {*/
#define EBCDIC_CHAR_CLOSEAC   ((unsigned char)0xD0) /* }*/
#define EBCDIC_CHAR_OPENPAR   ((unsigned char)0x4D) /* (*/
#define EBCDIC_CHAR_CLOSEPAR  ((unsigned char)0x5D) /* )*/
#define EBCDIC_CHAR_LESS      ((unsigned char)0x4C) /* < */
#define EBCDIC_CHAR_GREAT     ((unsigned char)0x6E) /* > */
#define EBCDIC_CHAR_SQUOTE    ((unsigned char)0x7D) /* ' */
#define EBCDIC_CHAR_DQUOTE    ((unsigned char)0x7F) /* " */
#define EBCDIC_CHAR_AT        ((unsigned char)0x7C) /* @ */
#define EBCDIC_CHAR_a         ((unsigned char)0x81) /* a */
#define EBCDIC_CHAR_b         ((unsigned char)0x82) /* b */
#define EBCDIC_CHAR_c         ((unsigned char)0x83) /* c */
#define EBCDIC_CHAR_d         ((unsigned char)0x84) /* d */
#define EBCDIC_CHAR_e         ((unsigned char)0x85) /* e */
#define EBCDIC_CHAR_f         ((unsigned char)0x86) /* f */
#define EBCDIC_CHAR_g         ((unsigned char)0x87) /* g */
#define EBCDIC_CHAR_h         ((unsigned char)0x88) /* h */
#define EBCDIC_CHAR_i         ((unsigned char)0x89) /* i */
#define EBCDIC_CHAR_j         ((unsigned char)0x91) /* j */
#define EBCDIC_CHAR_k         ((unsigned char)0x92) /* k */
#define EBCDIC_CHAR_l         ((unsigned char)0x93) /* l */
#define EBCDIC_CHAR_m         ((unsigned char)0x94) /* m */
#define EBCDIC_CHAR_n         ((unsigned char)0x95) /* n */
#define EBCDIC_CHAR_o         ((unsigned char)0x96) /* o */
#define EBCDIC_CHAR_p         ((unsigned char)0x97) /* p */
#define EBCDIC_CHAR_q         ((unsigned char)0x98) /* q */
#define EBCDIC_CHAR_r         ((unsigned char)0x99) /* r */
#define EBCDIC_CHAR_s         ((unsigned char)0xA2) /* s */
#define EBCDIC_CHAR_t         ((unsigned char)0xA3) /* t */
#define EBCDIC_CHAR_u         ((unsigned char)0xA4) /* u */
#define EBCDIC_CHAR_v         ((unsigned char)0xA5) /* v */
#define EBCDIC_CHAR_w         ((unsigned char)0xA6) /* w */
#define EBCDIC_CHAR_x         ((unsigned char)0xA7) /* x */
#define EBCDIC_CHAR_y         ((unsigned char)0xA8) /* y */
#define EBCDIC_CHAR_z         ((unsigned char)0xA9) /* z */
/*CIT_END_ENTERPRISE*/

/* ASCII CHARACTER SET */
#define ASCII_CHAR_0         ((unsigned char)0x30)
#define ASCII_CHAR_1         ((unsigned char)0x31)
#define ASCII_CHAR_2         ((unsigned char)0x32)
#define ASCII_CHAR_3         ((unsigned char)0x33)
#define ASCII_CHAR_4         ((unsigned char)0x34)
#define ASCII_CHAR_5         ((unsigned char)0x35)
#define ASCII_CHAR_6         ((unsigned char)0x36)
#define ASCII_CHAR_7         ((unsigned char)0x37)
#define ASCII_CHAR_8         ((unsigned char)0x38)
#define ASCII_CHAR_9         ((unsigned char)0x39)
#define ASCII_CHAR_SP        ((unsigned char)0x20)
#define ASCII_CHAR_TAB       ((unsigned char)0x09)
#define ASCII_CHAR_LF        ((unsigned char)0x0A)
#define ASCII_CHAR_VT        ((unsigned char)0x0B)
#define ASCII_CHAR_FF        ((unsigned char)0x0C)
#define ASCII_CHAR_CR        ((unsigned char)0x0D)
#define ASCII_CHAR_PLUS      ((unsigned char)0x2B)
#define ASCII_CHAR_MINUS     ((unsigned char)0x2D)
#define ASCII_CHAR_DOT       ((unsigned char)0x2E) /* . */
#define ASCII_CHAR_COMMA     ((unsigned char)0x2C) /* , */
#define ASCII_CHAR_DOLLAR    ((unsigned char)0x24) /* $ */
#define ASCII_CHAR_AMP       ((unsigned char)0x26) /* & */ /* 0x25 % */
#define ASCII_CHAR_QUESTION  ((unsigned char)0x3F) /* ? */
#define ASCII_CHAR_STAR      ((unsigned char)0x2A) /* * */
#define ASCII_CHAR_SLASH     ((unsigned char)0x2F) /* / */
#define ASCII_CHAR_A         ((unsigned char)0x41) /* A */
#define ASCII_CHAR_B         ((unsigned char)0x42) /* B */
#define ASCII_CHAR_C         ((unsigned char)0x43) /* C */
#define ASCII_CHAR_D         ((unsigned char)0x44) /* D */
#define ASCII_CHAR_E         ((unsigned char)0x45) /* E */
#define ASCII_CHAR_F         ((unsigned char)0x46) /* F */
#define ASCII_CHAR_G         ((unsigned char)0x47) /* G */
#define ASCII_CHAR_H         ((unsigned char)0x48) /* H */
#define ASCII_CHAR_I         ((unsigned char)0x49) /* I */
#define ASCII_CHAR_J         ((unsigned char)0x4A) /* J */
#define ASCII_CHAR_K         ((unsigned char)0x4B) /* K */
#define ASCII_CHAR_L         ((unsigned char)0x4C) /* L */
#define ASCII_CHAR_M         ((unsigned char)0x4D) /* M */
#define ASCII_CHAR_N         ((unsigned char)0x4E) /* N */
#define ASCII_CHAR_O         ((unsigned char)0x4F) /* O */
#define ASCII_CHAR_P         ((unsigned char)0x50) /* P */
#define ASCII_CHAR_Q         ((unsigned char)0x51) /* Q */
#define ASCII_CHAR_R         ((unsigned char)0x52) /* R */
#define ASCII_CHAR_S         ((unsigned char)0x53) /* S */
#define ASCII_CHAR_T         ((unsigned char)0x54) /* T */
#define ASCII_CHAR_U         ((unsigned char)0x55) /* U */
#define ASCII_CHAR_V         ((unsigned char)0x56) /* V */
#define ASCII_CHAR_W         ((unsigned char)0x57) /* W */
#define ASCII_CHAR_X         ((unsigned char)0x58) /* X */
#define ASCII_CHAR_Y         ((unsigned char)0x59) /* Y */
#define ASCII_CHAR_Z         ((unsigned char)0x5A) /* Z */
#define ASCII_CHAR_OPENTAB   ((unsigned char)0x5B) /* [ */
#define ASCII_CHAR_BACKSLASH ((unsigned char)0x5C) /* \ */
#define ASCII_CHAR_CLOSETAB  ((unsigned char)0x5D) /* ] */
#define ASCII_CHAR_CARET     ((unsigned char)0x5E) /* ^ */
#define ASCII_CHAR_UNDER     ((unsigned char)0x5F) /* _ */
#define ASCII_CHAR_BACKCOTE  ((unsigned char)0x60) /* ` */
#define ASCII_CHAR_OPENAC    ((unsigned char)0x7B) /* { */
#define ASCII_CHAR_CLOSEAC   ((unsigned char)0x7D) /* } */
#define ASCII_CHAR_OPENPAR   ((unsigned char)0x28) /* ( */
#define ASCII_CHAR_CLOSEPAR  ((unsigned char)0x29) /* ) */
#define ASCII_CHAR_LESS      ((unsigned char)0x3C) /* < */
#define ASCII_CHAR_GREAT     ((unsigned char)0x3E) /* > */
#define ASCII_CHAR_AT        ((unsigned char)0x40) /* @ */
#define ASCII_CHAR_SQUOTE    ((unsigned char)0x27) /* ' */
#define ASCII_CHAR_DQUOTE    ((unsigned char)0x22) /* " */
#define ASCII_CHAR_a         ((unsigned char)0x61) /* a */
#define ASCII_CHAR_b         ((unsigned char)0x62) /* b */
#define ASCII_CHAR_c         ((unsigned char)0x63) /* c */
#define ASCII_CHAR_d         ((unsigned char)0x64) /* d */
#define ASCII_CHAR_e         ((unsigned char)0x65) /* e */
#define ASCII_CHAR_f         ((unsigned char)0x66) /* f */
#define ASCII_CHAR_g         ((unsigned char)0x67) /* g */
#define ASCII_CHAR_h         ((unsigned char)0x68) /* h */
#define ASCII_CHAR_i         ((unsigned char)0x69) /* i */
#define ASCII_CHAR_j         ((unsigned char)0x6A) /* j */
#define ASCII_CHAR_k         ((unsigned char)0x6B) /* k */
#define ASCII_CHAR_l         ((unsigned char)0x6C) /* l */
#define ASCII_CHAR_m         ((unsigned char)0x6D) /* m */
#define ASCII_CHAR_n         ((unsigned char)0x6E) /* n */
#define ASCII_CHAR_o         ((unsigned char)0x6F) /* o */
#define ASCII_CHAR_p         ((unsigned char)0x70) /* p */
#define ASCII_CHAR_q         ((unsigned char)0x71) /* q */
#define ASCII_CHAR_r         ((unsigned char)0x72) /* r */
#define ASCII_CHAR_s         ((unsigned char)0x73) /* s */
#define ASCII_CHAR_t         ((unsigned char)0x74) /* t */
#define ASCII_CHAR_u         ((unsigned char)0x75) /* u */
#define ASCII_CHAR_v         ((unsigned char)0x76) /* v */
#define ASCII_CHAR_w         ((unsigned char)0x77) /* w */
#define ASCII_CHAR_x         ((unsigned char)0x78) /* x */
#define ASCII_CHAR_y         ((unsigned char)0x79) /* y */
#define ASCII_CHAR_z         ((unsigned char)0x7A) /* z */

/*CIT_BEGIN_ENTERPRISE*/
#if !defined(CIT_DUAL_CHARSET)
#if defined(CIT_EBCDIC_CHARSET_DEFAULT)

#define CHAR_0         EBCDIC_CHAR_0         
#define CHAR_1         EBCDIC_CHAR_1         
#define CHAR_2         EBCDIC_CHAR_2         
#define CHAR_3         EBCDIC_CHAR_3         
#define CHAR_4         EBCDIC_CHAR_4         
#define CHAR_5         EBCDIC_CHAR_5         
#define CHAR_6         EBCDIC_CHAR_6         
#define CHAR_7         EBCDIC_CHAR_7         
#define CHAR_8         EBCDIC_CHAR_8         
#define CHAR_9         EBCDIC_CHAR_9         
#define CHAR_SP        EBCDIC_CHAR_SP        
#define CHAR_TAB       EBCDIC_CHAR_TAB       
#define CHAR_LF        EBCDIC_CHAR_LF        
#define CHAR_VT        EBCDIC_CHAR_VT        
#define CHAR_FF        EBCDIC_CHAR_FF        
#define CHAR_CR        EBCDIC_CHAR_CR        
#define CHAR_PLUS      EBCDIC_CHAR_PLUS      
#define CHAR_MINUS     EBCDIC_CHAR_MINUS     
#define CHAR_DOT       EBCDIC_CHAR_DOT       
#define CHAR_COMMA     EBCDIC_CHAR_COMMA     
#define CHAR_DOLLAR    EBCDIC_CHAR_DOLLAR    /* $ */
#define CHAR_AMP       EBCDIC_CHAR_AMP       /* & */
#define CHAR_QUESTION  EBCDIC_CHAR_QUESTION  /* ? */
#define CHAR_STAR      EBCDIC_CHAR_STAR      /* * */
#define CHAR_SLASH     EBCDIC_CHAR_SLASH     /* / */
#define CHAR_A         EBCDIC_CHAR_A         /* A */
#define CHAR_B         EBCDIC_CHAR_B         /* B */
#define CHAR_C         EBCDIC_CHAR_C         /* C */
#define CHAR_D         EBCDIC_CHAR_D         /* D */
#define CHAR_E         EBCDIC_CHAR_E         /* E */
#define CHAR_F         EBCDIC_CHAR_F         /* F */
#define CHAR_G         EBCDIC_CHAR_G         /* G */
#define CHAR_H         EBCDIC_CHAR_H         /* H */
#define CHAR_I         EBCDIC_CHAR_I         /* I */
#define CHAR_J         EBCDIC_CHAR_J         /* J */
#define CHAR_K         EBCDIC_CHAR_K         /* K */
#define CHAR_L         EBCDIC_CHAR_L         /* L */
#define CHAR_M         EBCDIC_CHAR_M         /* M */
#define CHAR_N         EBCDIC_CHAR_N         /* N */
#define CHAR_O         EBCDIC_CHAR_O         /* O */
#define CHAR_P         EBCDIC_CHAR_P         /* P */
#define CHAR_Q         EBCDIC_CHAR_Q         /* Q */
#define CHAR_R         EBCDIC_CHAR_R         /* R */
#define CHAR_S         EBCDIC_CHAR_S         /* S */
#define CHAR_T         EBCDIC_CHAR_T         /* T */
#define CHAR_U         EBCDIC_CHAR_U         /* U */
#define CHAR_V         EBCDIC_CHAR_V         /* V */
#define CHAR_W         EBCDIC_CHAR_W         /* W */
#define CHAR_X         EBCDIC_CHAR_X         /* X */
#define CHAR_Y         EBCDIC_CHAR_Y         /* Y */
#define CHAR_Z         EBCDIC_CHAR_Z         /* Z */
#define CHAR_OPENTAB   EBCDIC_CHAR_OPENTAB   /* [ */
#define CHAR_BACKSLASH EBCDIC_CHAR_BACKSLASH /* \ */
#define CHAR_CLOSETAB  EBCDIC_CHAR_CLOSETAB  /* ] */
#define CHAR_CARET     EBCDIC_CHAR_CARET     /* ^ */
#define CHAR_UNDER     EBCDIC_CHAR_UNDER     /* _ */
#define CHAR_BACKCOTE  EBCDIC_CHAR_BACKCOTE  /* ` */
#define CHAR_OPENAC    EBCDIC_CHAR_OPENAC    /* {*/
#define CHAR_CLOSEAC   EBCDIC_CHAR_CLOSEAC   /* }*/
#define CHAR_OPENPAR   EBCDIC_CHAR_OPENPAR   /* (*/
#define CHAR_CLOSEPAR  EBCDIC_CHAR_CLOSEPAR  /* )*/
#define CHAR_LESS      EBCDIC_CHAR_LESS      /* < */
#define CHAR_GREAT     EBCDIC_CHAR_GREAT     /* > */
#define CHAR_SQUOTE    EBCDIC_CHAR_SQUOTE    /* ' */
#define CHAR_DQUOTE    EBCDIC_CHAR_DQUOTE    /* " */
#define CHAR_AT        EBCDIC_CHAR_AT        /* @ */
#define CHAR_a         EBCDIC_CHAR_a         /* a */
#define CHAR_b         EBCDIC_CHAR_b         /* b */
#define CHAR_c         EBCDIC_CHAR_c         /* c */
#define CHAR_d         EBCDIC_CHAR_d         /* d */
#define CHAR_e         EBCDIC_CHAR_e         /* e */
#define CHAR_f         EBCDIC_CHAR_f         /* f */
#define CHAR_g         EBCDIC_CHAR_g         /* g */
#define CHAR_h         EBCDIC_CHAR_h         /* h */
#define CHAR_i         EBCDIC_CHAR_i         /* i */
#define CHAR_j         EBCDIC_CHAR_j         /* j */
#define CHAR_k         EBCDIC_CHAR_k         /* k */
#define CHAR_l         EBCDIC_CHAR_l         /* l */
#define CHAR_m         EBCDIC_CHAR_m         /* m */
#define CHAR_n         EBCDIC_CHAR_n         /* n */
#define CHAR_o         EBCDIC_CHAR_o         /* o */
#define CHAR_p         EBCDIC_CHAR_p         /* p */
#define CHAR_q         EBCDIC_CHAR_q         /* q */
#define CHAR_r         EBCDIC_CHAR_r         /* r */
#define CHAR_s         EBCDIC_CHAR_s         /* s */
#define CHAR_t         EBCDIC_CHAR_t         /* t */
#define CHAR_u         EBCDIC_CHAR_u         /* u */
#define CHAR_v         EBCDIC_CHAR_v         /* v */
#define CHAR_w         EBCDIC_CHAR_w         /* w */
#define CHAR_x         EBCDIC_CHAR_x         /* x */
#define CHAR_y         EBCDIC_CHAR_y         /* y */
#define CHAR_z         EBCDIC_CHAR_z         /* z */

#else /*defined(CIT_EBCDIC_CHARSET_DEFAULT)*/
/*CIT_END_ENTERPRISE*/

#define CHAR_0         ASCII_CHAR_0         
#define CHAR_1         ASCII_CHAR_1         
#define CHAR_2         ASCII_CHAR_2         
#define CHAR_3         ASCII_CHAR_3         
#define CHAR_4         ASCII_CHAR_4         
#define CHAR_5         ASCII_CHAR_5         
#define CHAR_6         ASCII_CHAR_6         
#define CHAR_7         ASCII_CHAR_7         
#define CHAR_8         ASCII_CHAR_8         
#define CHAR_9         ASCII_CHAR_9         
#define CHAR_SP        ASCII_CHAR_SP        
#define CHAR_TAB       ASCII_CHAR_TAB       
#define CHAR_LF        ASCII_CHAR_LF        
#define CHAR_VT        ASCII_CHAR_VT        
#define CHAR_FF        ASCII_CHAR_FF        
#define CHAR_CR        ASCII_CHAR_CR        
#define CHAR_PLUS      ASCII_CHAR_PLUS      
#define CHAR_MINUS     ASCII_CHAR_MINUS     
#define CHAR_DOT       ASCII_CHAR_DOT       
#define CHAR_COMMA     ASCII_CHAR_COMMA     
#define CHAR_DOLLAR    ASCII_CHAR_DOLLAR    /* $ */
#define CHAR_AMP       ASCII_CHAR_AMP       /* & */
#define CHAR_QUESTION  ASCII_CHAR_QUESTION  /* ? */
#define CHAR_STAR      ASCII_CHAR_STAR      /* * */
#define CHAR_SLASH     ASCII_CHAR_SLASH     /* / */
#define CHAR_A         ASCII_CHAR_A         /* A */
#define CHAR_B         ASCII_CHAR_B         /* B */
#define CHAR_C         ASCII_CHAR_C         /* C */
#define CHAR_D         ASCII_CHAR_D         /* D */
#define CHAR_E         ASCII_CHAR_E         /* E */
#define CHAR_F         ASCII_CHAR_F         /* F */
#define CHAR_G         ASCII_CHAR_G         /* G */
#define CHAR_H         ASCII_CHAR_H         /* H */
#define CHAR_I         ASCII_CHAR_I         /* I */
#define CHAR_J         ASCII_CHAR_J         /* J */
#define CHAR_K         ASCII_CHAR_K         /* K */
#define CHAR_L         ASCII_CHAR_L         /* L */
#define CHAR_M         ASCII_CHAR_M         /* M */
#define CHAR_N         ASCII_CHAR_N         /* N */
#define CHAR_O         ASCII_CHAR_O         /* O */
#define CHAR_P         ASCII_CHAR_P         /* P */
#define CHAR_Q         ASCII_CHAR_Q         /* Q */
#define CHAR_R         ASCII_CHAR_R         /* R */
#define CHAR_S         ASCII_CHAR_S         /* S */
#define CHAR_T         ASCII_CHAR_T         /* T */
#define CHAR_U         ASCII_CHAR_U         /* U */
#define CHAR_V         ASCII_CHAR_V         /* V */
#define CHAR_W         ASCII_CHAR_W         /* W */
#define CHAR_X         ASCII_CHAR_X         /* X */
#define CHAR_Y         ASCII_CHAR_Y         /* Y */
#define CHAR_Z         ASCII_CHAR_Z         /* Z */
#define CHAR_OPENTAB   ASCII_CHAR_OPENTAB   /* [ */
#define CHAR_BACKSLASH ASCII_CHAR_BACKSLASH /* \ */
#define CHAR_CLOSETAB  ASCII_CHAR_CLOSETAB  /* ] */
#define CHAR_CARET     ASCII_CHAR_CARET     /* ^ */
#define CHAR_UNDER     ASCII_CHAR_UNDER     /* _ */
#define CHAR_BACKCOTE  ASCII_CHAR_BACKCOTE  /* ` */
#define CHAR_OPENAC    ASCII_CHAR_OPENAC    /* {*/
#define CHAR_CLOSEAC   ASCII_CHAR_CLOSEAC   /* }*/
#define CHAR_OPENPAR   ASCII_CHAR_OPENPAR   /* (*/
#define CHAR_CLOSEPAR  ASCII_CHAR_CLOSEPAR  /* )*/
#define CHAR_LESS      ASCII_CHAR_LESS      /* < */
#define CHAR_GREAT     ASCII_CHAR_GREAT     /* > */
#define CHAR_SQUOTE    ASCII_CHAR_SQUOTE    /* ' */
#define CHAR_DQUOTE    ASCII_CHAR_DQUOTE    /* " */
#define CHAR_AT        ASCII_CHAR_AT        /* @ */
#define CHAR_a         ASCII_CHAR_a         /* a */
#define CHAR_b         ASCII_CHAR_b         /* b */
#define CHAR_c         ASCII_CHAR_c         /* c */
#define CHAR_d         ASCII_CHAR_d         /* d */
#define CHAR_e         ASCII_CHAR_e         /* e */
#define CHAR_f         ASCII_CHAR_f         /* f */
#define CHAR_g         ASCII_CHAR_g         /* g */
#define CHAR_h         ASCII_CHAR_h         /* h */
#define CHAR_i         ASCII_CHAR_i         /* i */
#define CHAR_j         ASCII_CHAR_j         /* j */
#define CHAR_k         ASCII_CHAR_k         /* k */
#define CHAR_l         ASCII_CHAR_l         /* l */
#define CHAR_m         ASCII_CHAR_m         /* m */
#define CHAR_n         ASCII_CHAR_n         /* n */
#define CHAR_o         ASCII_CHAR_o         /* o */
#define CHAR_p         ASCII_CHAR_p         /* p */
#define CHAR_q         ASCII_CHAR_q         /* q */
#define CHAR_r         ASCII_CHAR_r         /* r */
#define CHAR_s         ASCII_CHAR_s         /* s */
#define CHAR_t         ASCII_CHAR_t         /* t */
#define CHAR_u         ASCII_CHAR_u         /* u */
#define CHAR_v         ASCII_CHAR_v         /* v */
#define CHAR_w         ASCII_CHAR_w         /* w */
#define CHAR_x         ASCII_CHAR_x         /* x */
#define CHAR_y         ASCII_CHAR_y         /* y */
#define CHAR_z         ASCII_CHAR_z         /* z */
/*CIT_BEGIN_ENTERPRISE*/

#endif /*defined(CIT_EBCDIC_CHARSET_DEFAULT)*/
/*CIT_END_ENTERPRISE*/
#define SWITCH_CHAR(a) (a) 
#define CASE_CHAR(a)  a 

/*CIT_BEGIN_ENTERPRISE*/

#else  /* !defined(CIT_DUAL_CHARSET)*/
#define CHAR_0         rtd->charset_map[ASCII_CHAR_0]         
#define CHAR_1         rtd->charset_map[ASCII_CHAR_1]         
#define CHAR_2         rtd->charset_map[ASCII_CHAR_2]         
#define CHAR_3         rtd->charset_map[ASCII_CHAR_3]         
#define CHAR_4         rtd->charset_map[ASCII_CHAR_4]         
#define CHAR_5         rtd->charset_map[ASCII_CHAR_5]         
#define CHAR_6         rtd->charset_map[ASCII_CHAR_6]         
#define CHAR_7         rtd->charset_map[ASCII_CHAR_7]         
#define CHAR_8         rtd->charset_map[ASCII_CHAR_8]         
#define CHAR_9         rtd->charset_map[ASCII_CHAR_9]         
#define CHAR_SP        rtd->charset_map[ASCII_CHAR_SP]        
#define CHAR_TAB       rtd->charset_map[ASCII_CHAR_TAB]       
#define CHAR_LF        rtd->charset_map[ASCII_CHAR_LF]        
#define CHAR_VT        rtd->charset_map[ASCII_CHAR_VT]        
#define CHAR_FF        rtd->charset_map[ASCII_CHAR_FF]        
#define CHAR_CR        rtd->charset_map[ASCII_CHAR_CR]        
#define CHAR_PLUS      rtd->charset_map[ASCII_CHAR_PLUS]      
#define CHAR_MINUS     rtd->charset_map[ASCII_CHAR_MINUS]     
#define CHAR_DOT       rtd->charset_map[ASCII_CHAR_DOT]       
#define CHAR_COMMA     rtd->charset_map[ASCII_CHAR_COMMA]     
#define CHAR_DOLLAR    rtd->charset_map[ASCII_CHAR_DOLLAR]    /* $ */
#define CHAR_AMP       rtd->charset_map[ASCII_CHAR_AMP]       /* & */
#define CHAR_QUESTION  rtd->charset_map[ASCII_CHAR_QUESTION]  /* ? */
#define CHAR_STAR      rtd->charset_map[ASCII_CHAR_STAR]      /* * */
#define CHAR_SLASH     rtd->charset_map[ASCII_CHAR_SLASH]     /* / */
#define CHAR_A         rtd->charset_map[ASCII_CHAR_A]         /* A */
#define CHAR_B         rtd->charset_map[ASCII_CHAR_B]         /* B */
#define CHAR_C         rtd->charset_map[ASCII_CHAR_C]         /* C */
#define CHAR_D         rtd->charset_map[ASCII_CHAR_D]         /* D */
#define CHAR_E         rtd->charset_map[ASCII_CHAR_E]         /* E */
#define CHAR_F         rtd->charset_map[ASCII_CHAR_F]         /* F */
#define CHAR_G         rtd->charset_map[ASCII_CHAR_G]         /* G */
#define CHAR_H         rtd->charset_map[ASCII_CHAR_H]         /* H */
#define CHAR_I         rtd->charset_map[ASCII_CHAR_I]         /* I */
#define CHAR_J         rtd->charset_map[ASCII_CHAR_J]         /* J */
#define CHAR_K         rtd->charset_map[ASCII_CHAR_K]         /* K */
#define CHAR_L         rtd->charset_map[ASCII_CHAR_L]         /* L */
#define CHAR_M         rtd->charset_map[ASCII_CHAR_M]         /* M */
#define CHAR_N         rtd->charset_map[ASCII_CHAR_N]         /* N */
#define CHAR_O         rtd->charset_map[ASCII_CHAR_O]         /* O */
#define CHAR_P         rtd->charset_map[ASCII_CHAR_P]         /* P */
#define CHAR_Q         rtd->charset_map[ASCII_CHAR_Q]         /* Q */
#define CHAR_R         rtd->charset_map[ASCII_CHAR_R]         /* R */
#define CHAR_S         rtd->charset_map[ASCII_CHAR_S]         /* S */
#define CHAR_T         rtd->charset_map[ASCII_CHAR_T]         /* T */
#define CHAR_U         rtd->charset_map[ASCII_CHAR_U]         /* U */
#define CHAR_V         rtd->charset_map[ASCII_CHAR_V]         /* V */
#define CHAR_W         rtd->charset_map[ASCII_CHAR_W]         /* W */
#define CHAR_X         rtd->charset_map[ASCII_CHAR_X]         /* X */
#define CHAR_Y         rtd->charset_map[ASCII_CHAR_Y]         /* Y */
#define CHAR_Z         rtd->charset_map[ASCII_CHAR_Z]         /* Z */
#define CHAR_OPENTAB   rtd->charset_map[ASCII_CHAR_OPENTAB]   /* [ */
#define CHAR_BACKSLASH rtd->charset_map[ASCII_CHAR_BACKSLASH] /* \ */
#define CHAR_CLOSETAB  rtd->charset_map[ASCII_CHAR_CLOSETAB]  /* ] */
#define CHAR_CARET     rtd->charset_map[ASCII_CHAR_CARET]     /* ^ */
#define CHAR_UNDER     rtd->charset_map[ASCII_CHAR_UNDER]     /* _ */
#define CHAR_BACKCOTE  rtd->charset_map[ASCII_CHAR_BACKCOTE]  /* ` */
#define CHAR_OPENAC    rtd->charset_map[ASCII_CHAR_OPENAC]    /* {*/
#define CHAR_CLOSEAC   rtd->charset_map[ASCII_CHAR_CLOSEAC]   /* }*/
#define CHAR_OPENPAR   rtd->charset_map[ASCII_CHAR_OPENPAR]   /* (*/
#define CHAR_CLOSEPAR  rtd->charset_map[ASCII_CHAR_CLOSEPAR]  /* )*/
#define CHAR_LESS      rtd->charset_map[ASCII_CHAR_LESS]      /* < */
#define CHAR_GREAT     rtd->charset_map[ASCII_CHAR_GREAT]     /* > */
#define CHAR_SQUOTE    rtd->charset_map[ASCII_CHAR_SQUOTE]    /* ' */
#define CHAR_DQUOTE    rtd->charset_map[ASCII_CHAR_DQUOTE]    /* " */
#define CHAR_AT        rtd->charset_map[ASCII_CHAR_AT]        /* @ */
#define CHAR_a         rtd->charset_map[ASCII_CHAR_a]         /* a */
#define CHAR_b         rtd->charset_map[ASCII_CHAR_b]         /* b */
#define CHAR_c         rtd->charset_map[ASCII_CHAR_c]         /* c */
#define CHAR_d         rtd->charset_map[ASCII_CHAR_d]         /* d */
#define CHAR_e         rtd->charset_map[ASCII_CHAR_e]         /* e */
#define CHAR_f         rtd->charset_map[ASCII_CHAR_f]         /* f */
#define CHAR_g         rtd->charset_map[ASCII_CHAR_g]         /* g */
#define CHAR_h         rtd->charset_map[ASCII_CHAR_h]         /* h */
#define CHAR_i         rtd->charset_map[ASCII_CHAR_i]         /* i */
#define CHAR_j         rtd->charset_map[ASCII_CHAR_j]         /* j */
#define CHAR_k         rtd->charset_map[ASCII_CHAR_k]         /* k */
#define CHAR_l         rtd->charset_map[ASCII_CHAR_l]         /* l */
#define CHAR_m         rtd->charset_map[ASCII_CHAR_m]         /* m */
#define CHAR_n         rtd->charset_map[ASCII_CHAR_n]         /* n */
#define CHAR_o         rtd->charset_map[ASCII_CHAR_o]         /* o */
#define CHAR_p         rtd->charset_map[ASCII_CHAR_p]         /* p */
#define CHAR_q         rtd->charset_map[ASCII_CHAR_q]         /* q */
#define CHAR_r         rtd->charset_map[ASCII_CHAR_r]         /* r */
#define CHAR_s         rtd->charset_map[ASCII_CHAR_s]         /* s */
#define CHAR_t         rtd->charset_map[ASCII_CHAR_t]         /* t */
#define CHAR_u         rtd->charset_map[ASCII_CHAR_u]         /* u */
#define CHAR_v         rtd->charset_map[ASCII_CHAR_v]         /* v */
#define CHAR_w         rtd->charset_map[ASCII_CHAR_w]         /* w */
#define CHAR_x         rtd->charset_map[ASCII_CHAR_x]         /* x */
#define CHAR_y         rtd->charset_map[ASCII_CHAR_y]         /* y */
#define CHAR_z         rtd->charset_map[ASCII_CHAR_z]         /* z */

#define SWITCH_CHAR(a)  (rtd->ebcdic_charset ? E2A_F(a) : a) 
        
#define CASE_CHAR(a)   ASCII_##a

#endif /* !defined(CIT_DUAL_CHARSET)*/
/*CIT_END_ENTERPRISE*/

typedef struct packed_value_t {
    unsigned char t;
    unsigned char u;
}packed_value_t;

static const packed_value_t packed_char_value_ASCII[] = {
/*          0x00                  0x01               0x02               0x03                0x04                0x05              0x06              0x07                0x08                0x09              0x0A              0x0b              0x0C             0x0D               0x0E              0x0F */
/*0x00*/  {ASCII_CHAR_0,ASCII_CHAR_0},   {ASCII_CHAR_0,ASCII_CHAR_1},   {ASCII_CHAR_0,ASCII_CHAR_2},   {ASCII_CHAR_0,ASCII_CHAR_3},   {ASCII_CHAR_0,ASCII_CHAR_4},   {ASCII_CHAR_0,ASCII_CHAR_5},   {ASCII_CHAR_0,ASCII_CHAR_6},   {ASCII_CHAR_0,ASCII_CHAR_7},   {ASCII_CHAR_0,ASCII_CHAR_8},   {ASCII_CHAR_0,ASCII_CHAR_9},   {ASCII_CHAR_0,ASCII_CHAR_0},  {ASCII_CHAR_0,ASCII_CHAR_0},  {ASCII_CHAR_0,ASCII_CHAR_0},  {ASCII_CHAR_0,ASCII_CHAR_0},  {ASCII_CHAR_0,ASCII_CHAR_0},  {ASCII_CHAR_0,ASCII_CHAR_0},   
/*0x10*/  {ASCII_CHAR_1,ASCII_CHAR_0},   {ASCII_CHAR_1,ASCII_CHAR_1},   {ASCII_CHAR_1,ASCII_CHAR_2},   {ASCII_CHAR_1,ASCII_CHAR_3},   {ASCII_CHAR_1,ASCII_CHAR_4},   {ASCII_CHAR_1,ASCII_CHAR_5},   {ASCII_CHAR_1,ASCII_CHAR_6},   {ASCII_CHAR_1,ASCII_CHAR_7},   {ASCII_CHAR_1,ASCII_CHAR_8},   {ASCII_CHAR_1,ASCII_CHAR_9},   {ASCII_CHAR_1,ASCII_CHAR_0},  {ASCII_CHAR_1,ASCII_CHAR_0},  {ASCII_CHAR_1,ASCII_CHAR_0},  {ASCII_CHAR_1,ASCII_CHAR_0},  {ASCII_CHAR_1,ASCII_CHAR_0},  {ASCII_CHAR_1,ASCII_CHAR_0},   
/*0x20*/  {ASCII_CHAR_2,ASCII_CHAR_0},   {ASCII_CHAR_2,ASCII_CHAR_1},   {ASCII_CHAR_2,ASCII_CHAR_2},   {ASCII_CHAR_2,ASCII_CHAR_3},   {ASCII_CHAR_2,ASCII_CHAR_4},   {ASCII_CHAR_2,ASCII_CHAR_5},   {ASCII_CHAR_2,ASCII_CHAR_6},   {ASCII_CHAR_2,ASCII_CHAR_7},   {ASCII_CHAR_2,ASCII_CHAR_8},   {ASCII_CHAR_2,ASCII_CHAR_9},   {ASCII_CHAR_2,ASCII_CHAR_0},  {ASCII_CHAR_2,ASCII_CHAR_0},  {ASCII_CHAR_2,ASCII_CHAR_0},  {ASCII_CHAR_2,ASCII_CHAR_0},  {ASCII_CHAR_2,ASCII_CHAR_0},  {ASCII_CHAR_2,ASCII_CHAR_0},   
/*0x30*/  {ASCII_CHAR_3,ASCII_CHAR_0},   {ASCII_CHAR_3,ASCII_CHAR_1},   {ASCII_CHAR_3,ASCII_CHAR_2},   {ASCII_CHAR_3,ASCII_CHAR_3},   {ASCII_CHAR_3,ASCII_CHAR_4},   {ASCII_CHAR_3,ASCII_CHAR_5},   {ASCII_CHAR_3,ASCII_CHAR_6},   {ASCII_CHAR_3,ASCII_CHAR_7},   {ASCII_CHAR_3,ASCII_CHAR_8},   {ASCII_CHAR_3,ASCII_CHAR_9},   {ASCII_CHAR_3,ASCII_CHAR_0},  {ASCII_CHAR_3,ASCII_CHAR_0},  {ASCII_CHAR_3,ASCII_CHAR_0},  {ASCII_CHAR_3,ASCII_CHAR_0},  {ASCII_CHAR_3,ASCII_CHAR_0},  {ASCII_CHAR_3,ASCII_CHAR_0},   
/*0x40*/  {ASCII_CHAR_4,ASCII_CHAR_0},   {ASCII_CHAR_4,ASCII_CHAR_1},   {ASCII_CHAR_4,ASCII_CHAR_2},   {ASCII_CHAR_4,ASCII_CHAR_3},   {ASCII_CHAR_4,ASCII_CHAR_4},   {ASCII_CHAR_4,ASCII_CHAR_5},   {ASCII_CHAR_4,ASCII_CHAR_6},   {ASCII_CHAR_4,ASCII_CHAR_7},   {ASCII_CHAR_4,ASCII_CHAR_8},   {ASCII_CHAR_4,ASCII_CHAR_9},   {ASCII_CHAR_4,ASCII_CHAR_0},  {ASCII_CHAR_4,ASCII_CHAR_0},  {ASCII_CHAR_4,ASCII_CHAR_0},  {ASCII_CHAR_4,ASCII_CHAR_0},  {ASCII_CHAR_4,ASCII_CHAR_0},  {ASCII_CHAR_4,ASCII_CHAR_0},   
/*0x50*/  {ASCII_CHAR_5,ASCII_CHAR_0},   {ASCII_CHAR_5,ASCII_CHAR_1},   {ASCII_CHAR_5,ASCII_CHAR_2},   {ASCII_CHAR_5,ASCII_CHAR_3},   {ASCII_CHAR_5,ASCII_CHAR_4},   {ASCII_CHAR_5,ASCII_CHAR_5},   {ASCII_CHAR_5,ASCII_CHAR_6},   {ASCII_CHAR_5,ASCII_CHAR_7},   {ASCII_CHAR_5,ASCII_CHAR_8},   {ASCII_CHAR_5,ASCII_CHAR_9},   {ASCII_CHAR_5,ASCII_CHAR_0},  {ASCII_CHAR_5,ASCII_CHAR_0},  {ASCII_CHAR_5,ASCII_CHAR_0},  {ASCII_CHAR_5,ASCII_CHAR_0},  {ASCII_CHAR_5,ASCII_CHAR_0},  {ASCII_CHAR_5,ASCII_CHAR_0},   
/*0x60*/  {ASCII_CHAR_6,ASCII_CHAR_0},   {ASCII_CHAR_6,ASCII_CHAR_1},   {ASCII_CHAR_6,ASCII_CHAR_2},   {ASCII_CHAR_6,ASCII_CHAR_3},   {ASCII_CHAR_6,ASCII_CHAR_4},   {ASCII_CHAR_6,ASCII_CHAR_5},   {ASCII_CHAR_6,ASCII_CHAR_6},   {ASCII_CHAR_6,ASCII_CHAR_7},   {ASCII_CHAR_6,ASCII_CHAR_8},   {ASCII_CHAR_6,ASCII_CHAR_9},   {ASCII_CHAR_6,ASCII_CHAR_0},  {ASCII_CHAR_6,ASCII_CHAR_0},  {ASCII_CHAR_6,ASCII_CHAR_0},  {ASCII_CHAR_6,ASCII_CHAR_0},  {ASCII_CHAR_6,ASCII_CHAR_0},  {ASCII_CHAR_6,ASCII_CHAR_0},   
/*0x70*/  {ASCII_CHAR_7,ASCII_CHAR_0},   {ASCII_CHAR_7,ASCII_CHAR_1},   {ASCII_CHAR_7,ASCII_CHAR_2},   {ASCII_CHAR_7,ASCII_CHAR_3},   {ASCII_CHAR_7,ASCII_CHAR_4},   {ASCII_CHAR_7,ASCII_CHAR_5},   {ASCII_CHAR_7,ASCII_CHAR_6},   {ASCII_CHAR_7,ASCII_CHAR_7},   {ASCII_CHAR_7,ASCII_CHAR_8},   {ASCII_CHAR_7,ASCII_CHAR_9},   {ASCII_CHAR_7,ASCII_CHAR_0},  {ASCII_CHAR_7,ASCII_CHAR_0},  {ASCII_CHAR_7,ASCII_CHAR_0},  {ASCII_CHAR_7,ASCII_CHAR_0},  {ASCII_CHAR_7,ASCII_CHAR_0},  {ASCII_CHAR_7,ASCII_CHAR_0},   
/*0x80*/  {ASCII_CHAR_8,ASCII_CHAR_0},   {ASCII_CHAR_8,ASCII_CHAR_1},   {ASCII_CHAR_8,ASCII_CHAR_2},   {ASCII_CHAR_8,ASCII_CHAR_3},   {ASCII_CHAR_8,ASCII_CHAR_4},   {ASCII_CHAR_8,ASCII_CHAR_5},   {ASCII_CHAR_8,ASCII_CHAR_6},   {ASCII_CHAR_8,ASCII_CHAR_7},   {ASCII_CHAR_8,ASCII_CHAR_8},   {ASCII_CHAR_8,ASCII_CHAR_9},   {ASCII_CHAR_8,ASCII_CHAR_0},  {ASCII_CHAR_8,ASCII_CHAR_0},  {ASCII_CHAR_8,ASCII_CHAR_0},  {ASCII_CHAR_8,ASCII_CHAR_0},  {ASCII_CHAR_8,ASCII_CHAR_0},  {ASCII_CHAR_8,ASCII_CHAR_0},   
/*0x90*/  {ASCII_CHAR_9,ASCII_CHAR_0},   {ASCII_CHAR_9,ASCII_CHAR_1},   {ASCII_CHAR_9,ASCII_CHAR_2},   {ASCII_CHAR_9,ASCII_CHAR_3},   {ASCII_CHAR_9,ASCII_CHAR_4},   {ASCII_CHAR_9,ASCII_CHAR_5},   {ASCII_CHAR_9,ASCII_CHAR_6},   {ASCII_CHAR_9,ASCII_CHAR_7},   {ASCII_CHAR_9,ASCII_CHAR_8},   {ASCII_CHAR_9,ASCII_CHAR_9},   {ASCII_CHAR_9,ASCII_CHAR_0},  {ASCII_CHAR_9,ASCII_CHAR_0},  {ASCII_CHAR_9,ASCII_CHAR_0},  {ASCII_CHAR_9,ASCII_CHAR_0},  {ASCII_CHAR_9,ASCII_CHAR_0},  {ASCII_CHAR_9,ASCII_CHAR_0},   
/*0xA0*/  {ASCII_CHAR_0,ASCII_CHAR_0},   {ASCII_CHAR_0,ASCII_CHAR_1},   {ASCII_CHAR_0,ASCII_CHAR_2},   {ASCII_CHAR_0,ASCII_CHAR_3},   {ASCII_CHAR_0,ASCII_CHAR_4},   {ASCII_CHAR_0,ASCII_CHAR_5},   {ASCII_CHAR_0,ASCII_CHAR_6},   {ASCII_CHAR_0,ASCII_CHAR_7},   {ASCII_CHAR_0,ASCII_CHAR_8},   {ASCII_CHAR_0,ASCII_CHAR_9},   {ASCII_CHAR_0,ASCII_CHAR_0},  {ASCII_CHAR_0,ASCII_CHAR_0},  {ASCII_CHAR_0,ASCII_CHAR_0},  {ASCII_CHAR_0,ASCII_CHAR_0},  {ASCII_CHAR_0,ASCII_CHAR_0},  {ASCII_CHAR_0,ASCII_CHAR_0},   
/*0xB0*/  {ASCII_CHAR_0,ASCII_CHAR_0},   {ASCII_CHAR_0,ASCII_CHAR_1},   {ASCII_CHAR_0,ASCII_CHAR_2},   {ASCII_CHAR_0,ASCII_CHAR_3},   {ASCII_CHAR_0,ASCII_CHAR_4},   {ASCII_CHAR_0,ASCII_CHAR_5},   {ASCII_CHAR_0,ASCII_CHAR_6},   {ASCII_CHAR_0,ASCII_CHAR_7},   {ASCII_CHAR_0,ASCII_CHAR_8},   {ASCII_CHAR_0,ASCII_CHAR_9},   {ASCII_CHAR_0,ASCII_CHAR_0},  {ASCII_CHAR_0,ASCII_CHAR_0},  {ASCII_CHAR_0,ASCII_CHAR_0},  {ASCII_CHAR_0,ASCII_CHAR_0},  {ASCII_CHAR_0,ASCII_CHAR_0},  {ASCII_CHAR_0,ASCII_CHAR_0},   
/*0xC0*/  {ASCII_CHAR_0,ASCII_CHAR_0},   {ASCII_CHAR_0,ASCII_CHAR_1},   {ASCII_CHAR_0,ASCII_CHAR_2},   {ASCII_CHAR_0,ASCII_CHAR_3},   {ASCII_CHAR_0,ASCII_CHAR_4},   {ASCII_CHAR_0,ASCII_CHAR_5},   {ASCII_CHAR_0,ASCII_CHAR_6},   {ASCII_CHAR_0,ASCII_CHAR_7},   {ASCII_CHAR_0,ASCII_CHAR_8},   {ASCII_CHAR_0,ASCII_CHAR_9},   {ASCII_CHAR_0,ASCII_CHAR_0},  {ASCII_CHAR_0,ASCII_CHAR_0},  {ASCII_CHAR_0,ASCII_CHAR_0},  {ASCII_CHAR_0,ASCII_CHAR_0},  {ASCII_CHAR_0,ASCII_CHAR_0},  {ASCII_CHAR_0,ASCII_CHAR_0},   
/*0xD0*/  {ASCII_CHAR_0,ASCII_CHAR_0},   {ASCII_CHAR_0,ASCII_CHAR_1},   {ASCII_CHAR_0,ASCII_CHAR_2},   {ASCII_CHAR_0,ASCII_CHAR_3},   {ASCII_CHAR_0,ASCII_CHAR_4},   {ASCII_CHAR_0,ASCII_CHAR_5},   {ASCII_CHAR_0,ASCII_CHAR_6},   {ASCII_CHAR_0,ASCII_CHAR_7},   {ASCII_CHAR_0,ASCII_CHAR_8},   {ASCII_CHAR_0,ASCII_CHAR_9},   {ASCII_CHAR_0,ASCII_CHAR_0},  {ASCII_CHAR_0,ASCII_CHAR_0},  {ASCII_CHAR_0,ASCII_CHAR_0},  {ASCII_CHAR_0,ASCII_CHAR_0},  {ASCII_CHAR_0,ASCII_CHAR_0},  {ASCII_CHAR_0,ASCII_CHAR_0},   
/*0xE0*/  {ASCII_CHAR_0,ASCII_CHAR_0},   {ASCII_CHAR_0,ASCII_CHAR_1},   {ASCII_CHAR_0,ASCII_CHAR_2},   {ASCII_CHAR_0,ASCII_CHAR_3},   {ASCII_CHAR_0,ASCII_CHAR_4},   {ASCII_CHAR_0,ASCII_CHAR_5},   {ASCII_CHAR_0,ASCII_CHAR_6},   {ASCII_CHAR_0,ASCII_CHAR_7},   {ASCII_CHAR_0,ASCII_CHAR_8},   {ASCII_CHAR_0,ASCII_CHAR_9},   {ASCII_CHAR_0,ASCII_CHAR_0},  {ASCII_CHAR_0,ASCII_CHAR_0},  {ASCII_CHAR_0,ASCII_CHAR_0},  {ASCII_CHAR_0,ASCII_CHAR_0},  {ASCII_CHAR_0,ASCII_CHAR_0},  {ASCII_CHAR_0,ASCII_CHAR_0},   
/*0xF0*/  {ASCII_CHAR_0,ASCII_CHAR_0},   {ASCII_CHAR_0,ASCII_CHAR_1},   {ASCII_CHAR_0,ASCII_CHAR_2},   {ASCII_CHAR_0,ASCII_CHAR_3},   {ASCII_CHAR_0,ASCII_CHAR_4},   {ASCII_CHAR_0,ASCII_CHAR_5},   {ASCII_CHAR_0,ASCII_CHAR_6},   {ASCII_CHAR_0,ASCII_CHAR_7},   {ASCII_CHAR_0,ASCII_CHAR_8},   {ASCII_CHAR_0,ASCII_CHAR_9},   {ASCII_CHAR_0,ASCII_CHAR_0},  {ASCII_CHAR_0,ASCII_CHAR_0},  {ASCII_CHAR_0,ASCII_CHAR_0},  {ASCII_CHAR_0,ASCII_CHAR_0},  {ASCII_CHAR_0,ASCII_CHAR_0},  {ASCII_CHAR_0,ASCII_CHAR_0},   
};

/*CIT_BEGIN_ENTERPRISE*/

static const packed_value_t packed_char_value_EBCDIC[] = {
/*          0x00                  0x01               0x02               0x03                0x04                0x05              0x06              0x07                0x08                0x09              0x0A              0x0b              0x0C             0x0D               0x0E              0x0F */
/*0x00*/  {EBCDIC_CHAR_0,EBCDIC_CHAR_0},   {EBCDIC_CHAR_0,EBCDIC_CHAR_1},   {EBCDIC_CHAR_0,EBCDIC_CHAR_2},   {EBCDIC_CHAR_0,EBCDIC_CHAR_3},   {EBCDIC_CHAR_0,EBCDIC_CHAR_4},   {EBCDIC_CHAR_0,EBCDIC_CHAR_5},   {EBCDIC_CHAR_0,EBCDIC_CHAR_6},   {EBCDIC_CHAR_0,EBCDIC_CHAR_7},   {EBCDIC_CHAR_0,EBCDIC_CHAR_8},   {EBCDIC_CHAR_0,EBCDIC_CHAR_9},   {EBCDIC_CHAR_0,EBCDIC_CHAR_0},  {EBCDIC_CHAR_0,EBCDIC_CHAR_0},  {EBCDIC_CHAR_0,EBCDIC_CHAR_0},  {EBCDIC_CHAR_0,EBCDIC_CHAR_0},  {EBCDIC_CHAR_0,EBCDIC_CHAR_0},  {EBCDIC_CHAR_0,EBCDIC_CHAR_0},   
/*0x10*/  {EBCDIC_CHAR_1,EBCDIC_CHAR_0},   {EBCDIC_CHAR_1,EBCDIC_CHAR_1},   {EBCDIC_CHAR_1,EBCDIC_CHAR_2},   {EBCDIC_CHAR_1,EBCDIC_CHAR_3},   {EBCDIC_CHAR_1,EBCDIC_CHAR_4},   {EBCDIC_CHAR_1,EBCDIC_CHAR_5},   {EBCDIC_CHAR_1,EBCDIC_CHAR_6},   {EBCDIC_CHAR_1,EBCDIC_CHAR_7},   {EBCDIC_CHAR_1,EBCDIC_CHAR_8},   {EBCDIC_CHAR_1,EBCDIC_CHAR_9},   {EBCDIC_CHAR_1,EBCDIC_CHAR_0},  {EBCDIC_CHAR_1,EBCDIC_CHAR_0},  {EBCDIC_CHAR_1,EBCDIC_CHAR_0},  {EBCDIC_CHAR_1,EBCDIC_CHAR_0},  {EBCDIC_CHAR_1,EBCDIC_CHAR_0},  {EBCDIC_CHAR_1,EBCDIC_CHAR_0},   
/*0x20*/  {EBCDIC_CHAR_2,EBCDIC_CHAR_0},   {EBCDIC_CHAR_2,EBCDIC_CHAR_1},   {EBCDIC_CHAR_2,EBCDIC_CHAR_2},   {EBCDIC_CHAR_2,EBCDIC_CHAR_3},   {EBCDIC_CHAR_2,EBCDIC_CHAR_4},   {EBCDIC_CHAR_2,EBCDIC_CHAR_5},   {EBCDIC_CHAR_2,EBCDIC_CHAR_6},   {EBCDIC_CHAR_2,EBCDIC_CHAR_7},   {EBCDIC_CHAR_2,EBCDIC_CHAR_8},   {EBCDIC_CHAR_2,EBCDIC_CHAR_9},   {EBCDIC_CHAR_2,EBCDIC_CHAR_0},  {EBCDIC_CHAR_2,EBCDIC_CHAR_0},  {EBCDIC_CHAR_2,EBCDIC_CHAR_0},  {EBCDIC_CHAR_2,EBCDIC_CHAR_0},  {EBCDIC_CHAR_2,EBCDIC_CHAR_0},  {EBCDIC_CHAR_2,EBCDIC_CHAR_0},   
/*0x30*/  {EBCDIC_CHAR_3,EBCDIC_CHAR_0},   {EBCDIC_CHAR_3,EBCDIC_CHAR_1},   {EBCDIC_CHAR_3,EBCDIC_CHAR_2},   {EBCDIC_CHAR_3,EBCDIC_CHAR_3},   {EBCDIC_CHAR_3,EBCDIC_CHAR_4},   {EBCDIC_CHAR_3,EBCDIC_CHAR_5},   {EBCDIC_CHAR_3,EBCDIC_CHAR_6},   {EBCDIC_CHAR_3,EBCDIC_CHAR_7},   {EBCDIC_CHAR_3,EBCDIC_CHAR_8},   {EBCDIC_CHAR_3,EBCDIC_CHAR_9},   {EBCDIC_CHAR_3,EBCDIC_CHAR_0},  {EBCDIC_CHAR_3,EBCDIC_CHAR_0},  {EBCDIC_CHAR_3,EBCDIC_CHAR_0},  {EBCDIC_CHAR_3,EBCDIC_CHAR_0},  {EBCDIC_CHAR_3,EBCDIC_CHAR_0},  {EBCDIC_CHAR_3,EBCDIC_CHAR_0},   
/*0x40*/  {EBCDIC_CHAR_4,EBCDIC_CHAR_0},   {EBCDIC_CHAR_4,EBCDIC_CHAR_1},   {EBCDIC_CHAR_4,EBCDIC_CHAR_2},   {EBCDIC_CHAR_4,EBCDIC_CHAR_3},   {EBCDIC_CHAR_4,EBCDIC_CHAR_4},   {EBCDIC_CHAR_4,EBCDIC_CHAR_5},   {EBCDIC_CHAR_4,EBCDIC_CHAR_6},   {EBCDIC_CHAR_4,EBCDIC_CHAR_7},   {EBCDIC_CHAR_4,EBCDIC_CHAR_8},   {EBCDIC_CHAR_4,EBCDIC_CHAR_9},   {EBCDIC_CHAR_4,EBCDIC_CHAR_0},  {EBCDIC_CHAR_4,EBCDIC_CHAR_0},  {EBCDIC_CHAR_4,EBCDIC_CHAR_0},  {EBCDIC_CHAR_4,EBCDIC_CHAR_0},  {EBCDIC_CHAR_4,EBCDIC_CHAR_0},  {EBCDIC_CHAR_4,EBCDIC_CHAR_0},   
/*0x50*/  {EBCDIC_CHAR_5,EBCDIC_CHAR_0},   {EBCDIC_CHAR_5,EBCDIC_CHAR_1},   {EBCDIC_CHAR_5,EBCDIC_CHAR_2},   {EBCDIC_CHAR_5,EBCDIC_CHAR_3},   {EBCDIC_CHAR_5,EBCDIC_CHAR_4},   {EBCDIC_CHAR_5,EBCDIC_CHAR_5},   {EBCDIC_CHAR_5,EBCDIC_CHAR_6},   {EBCDIC_CHAR_5,EBCDIC_CHAR_7},   {EBCDIC_CHAR_5,EBCDIC_CHAR_8},   {EBCDIC_CHAR_5,EBCDIC_CHAR_9},   {EBCDIC_CHAR_5,EBCDIC_CHAR_0},  {EBCDIC_CHAR_5,EBCDIC_CHAR_0},  {EBCDIC_CHAR_5,EBCDIC_CHAR_0},  {EBCDIC_CHAR_5,EBCDIC_CHAR_0},  {EBCDIC_CHAR_5,EBCDIC_CHAR_0},  {EBCDIC_CHAR_5,EBCDIC_CHAR_0},   
/*0x60*/  {EBCDIC_CHAR_6,EBCDIC_CHAR_0},   {EBCDIC_CHAR_6,EBCDIC_CHAR_1},   {EBCDIC_CHAR_6,EBCDIC_CHAR_2},   {EBCDIC_CHAR_6,EBCDIC_CHAR_3},   {EBCDIC_CHAR_6,EBCDIC_CHAR_4},   {EBCDIC_CHAR_6,EBCDIC_CHAR_5},   {EBCDIC_CHAR_6,EBCDIC_CHAR_6},   {EBCDIC_CHAR_6,EBCDIC_CHAR_7},   {EBCDIC_CHAR_6,EBCDIC_CHAR_8},   {EBCDIC_CHAR_6,EBCDIC_CHAR_9},   {EBCDIC_CHAR_6,EBCDIC_CHAR_0},  {EBCDIC_CHAR_6,EBCDIC_CHAR_0},  {EBCDIC_CHAR_6,EBCDIC_CHAR_0},  {EBCDIC_CHAR_6,EBCDIC_CHAR_0},  {EBCDIC_CHAR_6,EBCDIC_CHAR_0},  {EBCDIC_CHAR_6,EBCDIC_CHAR_0},   
/*0x70*/  {EBCDIC_CHAR_7,EBCDIC_CHAR_0},   {EBCDIC_CHAR_7,EBCDIC_CHAR_1},   {EBCDIC_CHAR_7,EBCDIC_CHAR_2},   {EBCDIC_CHAR_7,EBCDIC_CHAR_3},   {EBCDIC_CHAR_7,EBCDIC_CHAR_4},   {EBCDIC_CHAR_7,EBCDIC_CHAR_5},   {EBCDIC_CHAR_7,EBCDIC_CHAR_6},   {EBCDIC_CHAR_7,EBCDIC_CHAR_7},   {EBCDIC_CHAR_7,EBCDIC_CHAR_8},   {EBCDIC_CHAR_7,EBCDIC_CHAR_9},   {EBCDIC_CHAR_7,EBCDIC_CHAR_0},  {EBCDIC_CHAR_7,EBCDIC_CHAR_0},  {EBCDIC_CHAR_7,EBCDIC_CHAR_0},  {EBCDIC_CHAR_7,EBCDIC_CHAR_0},  {EBCDIC_CHAR_7,EBCDIC_CHAR_0},  {EBCDIC_CHAR_7,EBCDIC_CHAR_0},   
/*0x80*/  {EBCDIC_CHAR_8,EBCDIC_CHAR_0},   {EBCDIC_CHAR_8,EBCDIC_CHAR_1},   {EBCDIC_CHAR_8,EBCDIC_CHAR_2},   {EBCDIC_CHAR_8,EBCDIC_CHAR_3},   {EBCDIC_CHAR_8,EBCDIC_CHAR_4},   {EBCDIC_CHAR_8,EBCDIC_CHAR_5},   {EBCDIC_CHAR_8,EBCDIC_CHAR_6},   {EBCDIC_CHAR_8,EBCDIC_CHAR_7},   {EBCDIC_CHAR_8,EBCDIC_CHAR_8},   {EBCDIC_CHAR_8,EBCDIC_CHAR_9},   {EBCDIC_CHAR_8,EBCDIC_CHAR_0},  {EBCDIC_CHAR_8,EBCDIC_CHAR_0},  {EBCDIC_CHAR_8,EBCDIC_CHAR_0},  {EBCDIC_CHAR_8,EBCDIC_CHAR_0},  {EBCDIC_CHAR_8,EBCDIC_CHAR_0},  {EBCDIC_CHAR_8,EBCDIC_CHAR_0},   
/*0x90*/  {EBCDIC_CHAR_9,EBCDIC_CHAR_0},   {EBCDIC_CHAR_9,EBCDIC_CHAR_1},   {EBCDIC_CHAR_9,EBCDIC_CHAR_2},   {EBCDIC_CHAR_9,EBCDIC_CHAR_3},   {EBCDIC_CHAR_9,EBCDIC_CHAR_4},   {EBCDIC_CHAR_9,EBCDIC_CHAR_5},   {EBCDIC_CHAR_9,EBCDIC_CHAR_6},   {EBCDIC_CHAR_9,EBCDIC_CHAR_7},   {EBCDIC_CHAR_9,EBCDIC_CHAR_8},   {EBCDIC_CHAR_9,EBCDIC_CHAR_9},   {EBCDIC_CHAR_9,EBCDIC_CHAR_0},  {EBCDIC_CHAR_9,EBCDIC_CHAR_0},  {EBCDIC_CHAR_9,EBCDIC_CHAR_0},  {EBCDIC_CHAR_9,EBCDIC_CHAR_0},  {EBCDIC_CHAR_9,EBCDIC_CHAR_0},  {EBCDIC_CHAR_9,EBCDIC_CHAR_0},   
/*0xA0*/  {EBCDIC_CHAR_0,EBCDIC_CHAR_0},   {EBCDIC_CHAR_0,EBCDIC_CHAR_1},   {EBCDIC_CHAR_0,EBCDIC_CHAR_2},   {EBCDIC_CHAR_0,EBCDIC_CHAR_3},   {EBCDIC_CHAR_0,EBCDIC_CHAR_4},   {EBCDIC_CHAR_0,EBCDIC_CHAR_5},   {EBCDIC_CHAR_0,EBCDIC_CHAR_6},   {EBCDIC_CHAR_0,EBCDIC_CHAR_7},   {EBCDIC_CHAR_0,EBCDIC_CHAR_8},   {EBCDIC_CHAR_0,EBCDIC_CHAR_9},   {EBCDIC_CHAR_0,EBCDIC_CHAR_0},  {EBCDIC_CHAR_0,EBCDIC_CHAR_0},  {EBCDIC_CHAR_0,EBCDIC_CHAR_0},  {EBCDIC_CHAR_0,EBCDIC_CHAR_0},  {EBCDIC_CHAR_0,EBCDIC_CHAR_0},  {EBCDIC_CHAR_0,EBCDIC_CHAR_0},   
/*0xB0*/  {EBCDIC_CHAR_0,EBCDIC_CHAR_0},   {EBCDIC_CHAR_0,EBCDIC_CHAR_1},   {EBCDIC_CHAR_0,EBCDIC_CHAR_2},   {EBCDIC_CHAR_0,EBCDIC_CHAR_3},   {EBCDIC_CHAR_0,EBCDIC_CHAR_4},   {EBCDIC_CHAR_0,EBCDIC_CHAR_5},   {EBCDIC_CHAR_0,EBCDIC_CHAR_6},   {EBCDIC_CHAR_0,EBCDIC_CHAR_7},   {EBCDIC_CHAR_0,EBCDIC_CHAR_8},   {EBCDIC_CHAR_0,EBCDIC_CHAR_9},   {EBCDIC_CHAR_0,EBCDIC_CHAR_0},  {EBCDIC_CHAR_0,EBCDIC_CHAR_0},  {EBCDIC_CHAR_0,EBCDIC_CHAR_0},  {EBCDIC_CHAR_0,EBCDIC_CHAR_0},  {EBCDIC_CHAR_0,EBCDIC_CHAR_0},  {EBCDIC_CHAR_0,EBCDIC_CHAR_0},   
/*0xC0*/  {EBCDIC_CHAR_0,EBCDIC_CHAR_0},   {EBCDIC_CHAR_0,EBCDIC_CHAR_1},   {EBCDIC_CHAR_0,EBCDIC_CHAR_2},   {EBCDIC_CHAR_0,EBCDIC_CHAR_3},   {EBCDIC_CHAR_0,EBCDIC_CHAR_4},   {EBCDIC_CHAR_0,EBCDIC_CHAR_5},   {EBCDIC_CHAR_0,EBCDIC_CHAR_6},   {EBCDIC_CHAR_0,EBCDIC_CHAR_7},   {EBCDIC_CHAR_0,EBCDIC_CHAR_8},   {EBCDIC_CHAR_0,EBCDIC_CHAR_9},   {EBCDIC_CHAR_0,EBCDIC_CHAR_0},  {EBCDIC_CHAR_0,EBCDIC_CHAR_0},  {EBCDIC_CHAR_0,EBCDIC_CHAR_0},  {EBCDIC_CHAR_0,EBCDIC_CHAR_0},  {EBCDIC_CHAR_0,EBCDIC_CHAR_0},  {EBCDIC_CHAR_0,EBCDIC_CHAR_0},   
/*0xD0*/  {EBCDIC_CHAR_0,EBCDIC_CHAR_0},   {EBCDIC_CHAR_0,EBCDIC_CHAR_1},   {EBCDIC_CHAR_0,EBCDIC_CHAR_2},   {EBCDIC_CHAR_0,EBCDIC_CHAR_3},   {EBCDIC_CHAR_0,EBCDIC_CHAR_4},   {EBCDIC_CHAR_0,EBCDIC_CHAR_5},   {EBCDIC_CHAR_0,EBCDIC_CHAR_6},   {EBCDIC_CHAR_0,EBCDIC_CHAR_7},   {EBCDIC_CHAR_0,EBCDIC_CHAR_8},   {EBCDIC_CHAR_0,EBCDIC_CHAR_9},   {EBCDIC_CHAR_0,EBCDIC_CHAR_0},  {EBCDIC_CHAR_0,EBCDIC_CHAR_0},  {EBCDIC_CHAR_0,EBCDIC_CHAR_0},  {EBCDIC_CHAR_0,EBCDIC_CHAR_0},  {EBCDIC_CHAR_0,EBCDIC_CHAR_0},  {EBCDIC_CHAR_0,EBCDIC_CHAR_0},   
/*0xE0*/  {EBCDIC_CHAR_0,EBCDIC_CHAR_0},   {EBCDIC_CHAR_0,EBCDIC_CHAR_1},   {EBCDIC_CHAR_0,EBCDIC_CHAR_2},   {EBCDIC_CHAR_0,EBCDIC_CHAR_3},   {EBCDIC_CHAR_0,EBCDIC_CHAR_4},   {EBCDIC_CHAR_0,EBCDIC_CHAR_5},   {EBCDIC_CHAR_0,EBCDIC_CHAR_6},   {EBCDIC_CHAR_0,EBCDIC_CHAR_7},   {EBCDIC_CHAR_0,EBCDIC_CHAR_8},   {EBCDIC_CHAR_0,EBCDIC_CHAR_9},   {EBCDIC_CHAR_0,EBCDIC_CHAR_0},  {EBCDIC_CHAR_0,EBCDIC_CHAR_0},  {EBCDIC_CHAR_0,EBCDIC_CHAR_0},  {EBCDIC_CHAR_0,EBCDIC_CHAR_0},  {EBCDIC_CHAR_0,EBCDIC_CHAR_0},  {EBCDIC_CHAR_0,EBCDIC_CHAR_0},   
/*0xF0*/  {EBCDIC_CHAR_0,EBCDIC_CHAR_0},   {EBCDIC_CHAR_0,EBCDIC_CHAR_1},   {EBCDIC_CHAR_0,EBCDIC_CHAR_2},   {EBCDIC_CHAR_0,EBCDIC_CHAR_3},   {EBCDIC_CHAR_0,EBCDIC_CHAR_4},   {EBCDIC_CHAR_0,EBCDIC_CHAR_5},   {EBCDIC_CHAR_0,EBCDIC_CHAR_6},   {EBCDIC_CHAR_0,EBCDIC_CHAR_7},   {EBCDIC_CHAR_0,EBCDIC_CHAR_8},   {EBCDIC_CHAR_0,EBCDIC_CHAR_9},   {EBCDIC_CHAR_0,EBCDIC_CHAR_0},  {EBCDIC_CHAR_0,EBCDIC_CHAR_0},  {EBCDIC_CHAR_0,EBCDIC_CHAR_0},  {EBCDIC_CHAR_0,EBCDIC_CHAR_0},  {EBCDIC_CHAR_0,EBCDIC_CHAR_0},  {EBCDIC_CHAR_0,EBCDIC_CHAR_0},   
};

/*CIT_END_ENTERPRISE*/
#endif /*CIT_TYPES_H*/
