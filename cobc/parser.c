
/* A Bison parser, made by GNU Bison 2.4.1.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C
   
      Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.
   
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.
   
   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.4.1"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1

/* Using locations.  */
#define YYLSP_NEEDED 0



/* Copy the first part of user declarations.  */

/* Line 189 of yacc.c  */
#line 31 "parser.y"

#include "cobc.h"

#include <stdlib.h>
#include <string.h>
#include <libcob.h>

#include <libcob.h>
#include "tree.h"
#include <enterprise/a2e.h>

#define YYSTYPE                 cb_tree
#define yyerror                 cb_error
#define YYDEBUG                 1
#define YYERROR_VERBOSE         1

#define PENDING(x)              cb_warning (_("'%s' not implemented"), x)

#define emit_statement(x) \
  current_program->exec_list = cb_cons (x, current_program->exec_list)

#define push_expr(type, node) \
  current_expr = cb_build_list (cb_int (type), node, current_expr)

#define TERM_NONE               0
#define TERM_ACCEPT             1
#define TERM_ADD                2
#define TERM_CALL               3
#define TERM_COMPUTE            4
#define TERM_DELETE             5
#define TERM_DISPLAY            6
#define TERM_DIVIDE             7
#define TERM_EVALUATE           8
#define TERM_IF                 9
#define TERM_MULTIPLY           10
#define TERM_PERFORM            11
#define TERM_READ               12
#define TERM_RECEIVE            13
#define TERM_RETURN             14
#define TERM_REWRITE            15
#define TERM_SEARCH             16
#define TERM_START              17
#define TERM_STRING             18
#define TERM_SUBTRACT           19
#define TERM_UNSTRING           20
#define TERM_WRITE              21
#define TERM_CHAIN              22
#define TERM_EXHIBIT            23
#define TERM_XML                24
#define TERM_MAX                25

/* Global variables */

struct cb_program       *external_program_list= NULL;
struct cb_program       *current_program = NULL;
struct cb_statement     *current_statement = NULL;
struct cb_label         *current_section = NULL;
struct cb_label         *current_paragraph = NULL;
size_t                  functions_are_all = 0;
size_t                  check_unreached = 0;
int                     non_const_word = 0;
struct cb_field         *typedef_storage = NULL;

/* Local variables */

static struct cb_statement      *main_statement;

static cb_tree                  current_expr;
static struct cb_field          *current_field = NULL;
static struct cb_field          *description_field;
static struct cb_file           *current_file;

static enum cb_storage          current_storage;

static int                      call_mode;
static int                      size_mode;
static int                      rounded_used;

static cb_tree                  perform_stack = NULL;
static cb_tree                  qualifier = NULL;

static cb_tree                  display_fgc;
static cb_tree                  display_bgc;
static cb_tree                  display_line;
static cb_tree                  display_column;
static cb_tree                  display_upon;
static cb_tree                  display_advance;
static cb_tree                  display_x_list;
static cb_tree                  scroll;
static cb_tree                  save_tree_1;
static cb_tree                  save_tree_2;
static cb_tree                  dummy_tree;
static cb_tree                  acc_exception_field;
static cb_tree                  acc_timeout;
static cb_tree                  start_cond;
static int                      sort_input_proc;

/*saving variables*/
static cb_tree                  save_display_fgc;
static cb_tree                  save_display_bgc;
static cb_tree                  save_display_line;
static cb_tree                  save_display_column;
static cb_tree                  save_scroll;
static int                      save_dispattrs = 0;
static int                      save_dispprompt = 0;
/**/
static cb_tree                  current_file_type;
static size_t                   in_declaratives = 0;
static size_t                   current_linage = 0;
static size_t                   prog_end = 0;
static size_t                   use_global_ind = 0;
static size_t                   samearea = 1;
static size_t                   organized_seen = 0;
static size_t                   inspect_keyword = 0;
static int                      next_label_id = 0;
static int                      eval_level = 0;
static int                      eval_inc = 0;
static int                      eval_inc2 = 0;
static int                      depth = 0;
static int                      dispattrs = 0;
static int                      dispprompt = 0;
static struct cb_file           *linage_file;
static cb_tree                  next_label_list = NULL;
static char                     *stack_progid[32];
static int                      term_array[TERM_MAX];
static int                      eval_check[64][64];
static int                      exhibit_option;
/*static cb_tree                  eval_subject[64][64];*/

/* Static functions */

static void
BEGIN_STATEMENT_1 (const char *name, const size_t term, int checkpos)
{
    if (cb_warn_unreachable && check_unreached) {
        cb_warning (_("Unreachable statement"));
    }
    current_statement = cb_build_statement ((char *)name);
    CB_TREE (current_statement)->source_file = (unsigned char *)cb_lex_source_file.file_name;
    CB_TREE (current_statement)->source_line = cb_lex_source_file.line;
    CB_TREE (current_statement)->source_column = cb_lex_source_file.column; 
    emit_statement (CB_TREE (current_statement));
    if (term) {
        term_array[term]++;
    }
    main_statement = current_statement;
    current_program->field_pointer_count = 0;
    if (checkpos && CB_TREE (current_statement)->source_column <= 4) {
      cb_check_feature_x ( CB_TREE (current_statement), cb_syntax_ibm5_2, "Statement start at %d must start after column 12", CB_TREE (current_statement)->source_column + 7);
    }
}

#define BEGIN_STATEMENT(name,term) BEGIN_STATEMENT_1(name,term,1)

static void
BEGIN_IMPLICIT_STATEMENT (void)
{
    current_statement = cb_build_statement (NULL);
    main_statement->body = cb_list_add (main_statement->body,
                        CB_TREE (current_statement));
}

static void
emit_entry (const char *name, const int encode, cb_tree using_list, int call_conv, int module_entry)
{
    cb_tree     l;
    cb_tree     label;
    cb_tree     x;
    cb_tree     y;
    struct cb_field *f;
    int         parmnum;
    char        buff[256];
    struct cb_program *p;

    sprintf (buff, "E$%s", name);
    label = cb_build_label (cb_build_reference (buff), NULL);
    if (encode) {
        CB_LABEL (label)->name = (unsigned char *)(cb_encode_program_id (name));
        CB_LABEL (label)->orig_name = (unsigned char *)name;
    } else {
        CB_LABEL (label)->name = (unsigned char *)name;
        CB_LABEL (label)->orig_name = (unsigned char *)current_program->orig_source_name;
    }
    CB_LABEL (label)->need_begin = 1;
    CB_LABEL (label)->is_entry = 1;
    CB_LABEL (label)->is_module_entry = module_entry;
    CB_LABEL (label)->entry_call_convesion = call_conv;
    emit_statement (label);

    parmnum = 1;
    for (l = using_list; l; l = CB_CHAIN (l)) {
        x = CB_VALUE (l);
        if (x != cb_error_node && cb_ref (x) != cb_error_node) {
            f = CB_FIELD (cb_ref (x));
            if (f->level != 01 && f->level != 77) {
                cb_error_x (x, _("'%s' not level 01 or 77"), cb_name (x));
            }
            if (!current_program->flag_chained) {
                if (f->storage != CB_STORAGE_LINKAGE) {
                        /*COBOL-IT*/
                        if ( !cb_relaxed_syntax_check || 
                             !cb_check_feature_x (x, cb_syntax_ibm5_2, "USING field must be in LINKAGE")) {
                               cb_error_x (x, _("'%s' is not in LINKAGE SECTION"), cb_name (x));
                        } else {
                               /*COBOL-IT*/
                               /*Hack for MF to allow now linage section */
                               if (f->flag_external ){
                                        cb_error_x (x, _("PROCEDURE USING parameter '%s' is EXTERNAL"), cb_name (x));
                               } else {
                                        cb_warning_information (x, _("'%s' is not in LINKAGE SECTION"), cb_name (x));
                                        /*
                                        f->storage = CB_STORAGE_LINKAGE;
                                        f->flag_local = 1;
                                        f->flag_base = 1;
                                        */
                                        y = cb_build_filler ();
                                        cb_build_dummy_linkage(y,f->size);
                                        f = CB_FIELD (cb_ref (y));
                                        CB_VALUE (l) = y;
                                        emit_statement(cb_build_move_memcopy(y, x));
                               }
                        }
                }
                if (f->flag_item_based || f->flag_external) {
                    cb_error_x (x, _("'%s' can not be BASED/EXTERNAL"), cb_name (x));
                }
                f->flag_is_pdiv_parm = 1;
            } else {
                if (f->storage != CB_STORAGE_WORKING) {
                    cb_error_x (x, _("'%s' is not in WORKING-STORAGE SECTION"), cb_name (x));
                }
                f->flag_chained = 1;
                f->param_num = parmnum;
                parmnum++;
            }
            if (f->redefines) {
                if (CB_PURPOSE(l) != cb_int (CB_CALL_BY_REFERENCE) &&
                    CB_PURPOSE(l) != cb_int (CB_CALL_BY_DEFAULT) &&
                    CB_PURPOSE(l) != cb_int (CB_CALL_BY_DESCRIPTOR)) {
                    cb_error_x (x, _("'%s' REDEFINES field not allowed here"), cb_name (x));
                } else {
                    while(f->redefines) {
                        f = f->redefines;
                    }
                    CB_VALUE (l) = cb_build_field_reference (f, NULL);
                }
            } 
        }
    }

    /* Check dangling LINKAGE items */
    if (cb_warn_linkage) {
        for (f = current_program->linkage_storage; f; f = f->sister) {
            for (l = using_list; l; l = CB_CHAIN (l)) {
                x = CB_VALUE (l);
                if (x != cb_error_node && cb_ref (x) != cb_error_node) {
                    if (f == CB_FIELD (cb_ref (x))) {
                        break;
                    }
                }
            }
            if (!l && !f->redefines) {
                cb_warning (_("LINKAGE item '%s' is not a PROCEDURE USING parameter"), f->name);
            }
        }
    }

    /* check for duplicated entry*/
    for (p=current_program; p; p = p->next_program) {
        for (l = p->entry_list; l; l = CB_CHAIN (l)) {
            struct cb_entry *e = CB_ENTRY(CB_VALUE(l)); 
            if (strcasecmp ((const char *)name, (const char *)(CB_LABEL(e->label)->name)) == 0) {               
                if (current_statement) {
                    /* DO we have ENTRY with the same name than module file and != PRODID*/
                    if (source_demangle_name &&
                        (strcasecmp(p->program_id, source_demangle_name) != 0)) {
                       /* yes then change the "faque entry build from the module name to this entry*/
                       e->no_entry_function = 1;
                    } else {
                       cb_error_x (CB_TREE (current_statement), _("ENTRY '%s' duplicated"), name);
                    }
                } else {
                    cb_error (_("ENTRY '%s' duplicated or use -fno-module-name-entry"), name);
                }
            }
        }
    }

    current_program->entry_list = cb_list_add (current_program->entry_list,
                                       cb_build_entry(label,using_list,NULL, call_conv));
}

static void
terminator_warning (const size_t termid)
{
    check_unreached = 0;
    if (cb_warn_terminator && term_array[termid]) {
        cb_warning_x (CB_TREE (current_statement),
            _("%s statement not terminated by END-%s"),
            current_statement->name, current_statement->name);
    }
    if (term_array[termid]) {
        term_array[termid]--;
    }
}

/*
static void
terminator_error (void)
{
    check_unreached = 0;
    cb_error_x (CB_TREE (current_statement),
            _("%s statement not terminated by END-%s"),
            current_statement->name, current_statement->name);
}
*/
static void
terminator_clear (const size_t termid)
{
    check_unreached = 0;
    if (term_array[termid]) {
        term_array[termid]--;
    }
}

static int
literal_value (cb_tree x)
{
    if (x == cb_space) {
        return CHAR_SP;
    } else if (x == cb_zero) {
        return CHAR_0;
    } else if (x == cb_quote) {
        return cb_quote_char ; /* '"'; */
    } else if (x == cb_dirsep) {
        return *(CB_LITERAL(cb_dirsep)->data) ; /* '/'; */
        
    } else if (x == cb_null) {
        return 0;
    } else if (CB_TREE_CLASS (x) == CB_CLASS_NUMERIC) {
        return cb_get_int (x);
    } else {
        return CB_LITERAL (x)->data[0];
    }
}

static void
setup_use_file (struct cb_file *fileptr)
{
    struct cb_file  *newptr;

    if (fileptr->organization == COB_ORG_SORT) {
        cb_error (_("USE statement invalid for SORT file"));
    }
    if (fileptr->global) {
        newptr = cobc_malloc (sizeof(struct cb_file));
        *newptr = *fileptr;
        newptr->handler = current_section;
        newptr->handler_prog = current_program;
        if (!use_global_ind) {
            current_program->local_file_list =
                cb_list_add (current_program->local_file_list,
                         CB_TREE (newptr));
        } else {
            current_program->global_file_list =
                cb_list_add (current_program->global_file_list,
                         CB_TREE (newptr));
        }
    } else {
        fileptr->handler = current_section;
    }
}



/* Line 189 of yacc.c  */
#line 449 "parser.c"

/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif


/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     TOKEN_EOF = 0,
     ABEND = 258,
     ACCEPT = 259,
     ACCESS = 260,
     ADD = 261,
     ADDRESS = 262,
     ADVANCING = 263,
     AFTER = 264,
     ALL = 265,
     ALLOCATE = 266,
     ALPHABET = 267,
     ALPHABETIC = 268,
     ALPHABETIC_LOWER = 269,
     ALPHABETIC_UPPER = 270,
     ALPHANUMERIC = 271,
     ALPHANUMERIC_EDITED = 272,
     ALSO = 273,
     ALTER = 274,
     ALTERNATE = 275,
     AND = 276,
     ANY = 277,
     APPLY = 278,
     ARE = 279,
     AREA = 280,
     ARGUMENT_NUMBER = 281,
     ARGUMENT_VALUE = 282,
     AS = 283,
     ASCENDING = 284,
     ASSIGN = 285,
     AT = 286,
     AUTO = 287,
     AUTOMATIC = 288,
     BACKGROUND_COLOR = 289,
     BASED = 290,
     BEFORE = 291,
     BELL = 292,
     B_AND = 293,
     B_NOT = 294,
     B_OR = 295,
     B_XOR = 296,
     BINARY = 297,
     BINARY_C_LONG = 298,
     BINARY_CHAR = 299,
     BINARY_DOUBLE = 300,
     BINARY_LONG = 301,
     BINARY_SHORT = 302,
     BIT = 303,
     BLANK = 304,
     BLANK_LINE = 305,
     BLANK_SCREEN = 306,
     BLINK = 307,
     BLOCK = 308,
     BOTTOM = 309,
     BY = 310,
     BYTE_LENGTH = 311,
     CALL = 312,
     CANCEL = 313,
     CDECL = 314,
     CENTURY_DATE = 315,
     CENTURY_DAY = 316,
     CH = 317,
     CHAIN = 318,
     CHAINING = 319,
     CHANGED = 320,
     CHARACTER = 321,
     CHARACTERS = 322,
     CHECKPOINT = 323,
     CLASS = 324,
     CLOSE = 325,
     CODE = 326,
     CODE_SET = 327,
     COLLATING = 328,
     COL = 329,
     COLS = 330,
     COLUMN = 331,
     COLUMNS = 332,
     COMMA = 333,
     COMMAND_LINE = 334,
     COMMA_DELIM = 335,
     COMMIT = 336,
     COMMON = 337,
     COMP = 338,
     COMPUTE = 339,
     COMP_1 = 340,
     COMP_2 = 341,
     COMP_3 = 342,
     COMP_4 = 343,
     COMP_5 = 344,
     COMP_6 = 345,
     COMP_X = 346,
     CONCATENATE_FUNC = 347,
     CONFIGURATION = 348,
     CONSTANT = 349,
     CONTAINS = 350,
     CONTENT = 351,
     CONTINUE = 352,
     CONTROL = 353,
     CONTROLS = 354,
     CONVERT = 355,
     CONVERTING = 356,
     CORE_INDEX = 357,
     CORRESPONDING = 358,
     COUNT = 359,
     CRT = 360,
     CURRENCY = 361,
     CURRENT_DATE_FUNC = 362,
     CURSOR = 363,
     CYCLE = 364,
     DATA = 365,
     DATE = 366,
     DAY = 367,
     DAY_OF_WEEK = 368,
     DE = 369,
     DEBUGGING = 370,
     DECIMAL_POINT = 371,
     DECLARATIVES = 372,
     DEFAULT = 373,
     DELETE = 374,
     DELIMITED = 375,
     DELIMITER = 376,
     DEPENDING = 377,
     DESCENDING = 378,
     DESCRIPTOR = 379,
     DETAIL = 380,
     DIR_SEPARATOR = 381,
     DISK = 382,
     DISPLAY = 383,
     DISPLAY_1 = 384,
     DISPLAY_OF_FUNC = 385,
     DIVIDE = 386,
     DIVISION = 387,
     DOTWORD = 388,
     DOWN = 389,
     DUPLICATES = 390,
     DYNAMIC = 391,
     ECHO = 392,
     EBCDIC = 393,
     ELSE = 394,
     EMPTY_CHECK = 395,
     ENCODING = 396,
     END = 397,
     END_ACCEPT = 398,
     END_ADD = 399,
     END_CALL = 400,
     END_COMPUTE = 401,
     END_CHAIN = 402,
     END_DELETE = 403,
     END_DISPLAY = 404,
     END_DIVIDE = 405,
     END_EVALUATE = 406,
     END_EXHIBIT = 407,
     END_FUNCTION = 408,
     END_IF = 409,
     END_MULTIPLY = 410,
     END_PERFORM = 411,
     END_PROGRAM = 412,
     END_READ = 413,
     END_RETURN = 414,
     END_REWRITE = 415,
     END_SEARCH = 416,
     END_START = 417,
     END_STRING = 418,
     END_SUBTRACT = 419,
     END_UNSTRING = 420,
     END_WRITE = 421,
     END_XML = 422,
     ENTRY = 423,
     ENVIRONMENT = 424,
     ENVIRONMENT_NAME = 425,
     ENVIRONMENT_VALUE = 426,
     EOL = 427,
     EOP = 428,
     EOS = 429,
     EQUAL = 430,
     EQUALS = 431,
     ERASE = 432,
     ERROR = 433,
     ESCAPE = 434,
     EVALUATE = 435,
     EVENT_STATUS = 436,
     EXCEPTION = 437,
     EXCLUSIVE = 438,
     EXIT = 439,
     EXHIBIT = 440,
     EXTEND = 441,
     EXTERNAL = 442,
     FAILURE = 443,
     FD = 444,
     FILE_CONTROL = 445,
     FILE_ID = 446,
     FILLER = 447,
     FINAL = 448,
     FIRST = 449,
     FOOTING = 450,
     FOR = 451,
     FOREGROUND_COLOR = 452,
     FOREVER = 453,
     FREE = 454,
     FROM = 455,
     FULL = 456,
     FUNCTION = 457,
     FUNCTION_ID = 458,
     FUNCTION_NAME = 459,
     GE = 460,
     GENERATE = 461,
     GIVING = 462,
     GLOBAL = 463,
     GO = 464,
     GOBACK = 465,
     GREATER = 466,
     GROUP = 467,
     HEADING = 468,
     HIGHLIGHT = 469,
     HIGH_VALUE = 470,
     IDENTIFICATION = 471,
     IF = 472,
     IGNORE = 473,
     IGNORING = 474,
     IN = 475,
     INDEX = 476,
     INDEXED = 477,
     INDICATE = 478,
     INITIALIZE = 479,
     INITIALIZED = 480,
     INITIATE = 481,
     INPUT = 482,
     INPUT_OUTPUT = 483,
     INSPECT = 484,
     INTO = 485,
     INTRINSIC = 486,
     INVALID = 487,
     INVALID_KEY = 488,
     IS = 489,
     I_O = 490,
     I_O_CONTROL = 491,
     JUSTIFIED = 492,
     KEPT = 493,
     KEY = 494,
     LABEL = 495,
     LAST = 496,
     LE = 497,
     LEADING = 498,
     LEFT = 499,
     LENGTH = 500,
     LENGTH_CHECK = 501,
     LESS = 502,
     LIKE = 503,
     LIMIT = 504,
     LIMITS = 505,
     LINAGE = 506,
     LINAGE_COUNTER = 507,
     LINE = 508,
     LINES = 509,
     LINKAGE = 510,
     LITERAL = 511,
     LOCALE = 512,
     LOCALE_DT_FUNC = 513,
     LOCAL_STORAGE = 514,
     LOCK = 515,
     LOWER_CASE_FUNC = 516,
     LOWLIGHT = 517,
     LOW_VALUE = 518,
     MANUAL = 519,
     MEMORY = 520,
     MERGE = 521,
     MINUS = 522,
     MNEMONIC_NAME = 523,
     MODE = 524,
     MOVE = 525,
     MULTIPLE = 526,
     MULTIPLY = 527,
     NAMED = 528,
     NATIONAL = 529,
     NATIONAL_EDITED = 530,
     NATIONAL_OF_FUNC = 531,
     NATIVE = 532,
     NE = 533,
     NEGATIVE = 534,
     NEXT = 535,
     NO = 536,
     NOT = 537,
     NOT_END = 538,
     NOT_EOP = 539,
     NOT_EXCEPTION = 540,
     NOT_INVALID_KEY = 541,
     NOT_OVERFLOW = 542,
     NOT_SIZE_ERROR = 543,
     NO_ADVANCING = 544,
     NUMBER = 545,
     NUMBERS = 546,
     NUMERIC = 547,
     NUMERIC_EDITED = 548,
     NUMVALC_FUNC = 549,
     OBJECT_COMPUTER = 550,
     OCCURS = 551,
     OF = 552,
     OFF = 553,
     OMITTED = 554,
     ON = 555,
     ONLY = 556,
     OPEN = 557,
     OPTIONAL = 558,
     OR = 559,
     ORDER = 560,
     ORGANIZATION = 561,
     OTHER = 562,
     OUTPUT = 563,
     COB_OVERFLOW = 564,
     OVERLINE = 565,
     PACKED_DECIMAL = 566,
     PADDING = 567,
     PAGE = 568,
     PARAGRAPH = 569,
     PARSE = 570,
     PERFORM = 571,
     PICTURE = 572,
     PLUS = 573,
     POINTER = 574,
     POSITION = 575,
     POSITIVE = 576,
     PRESENT = 577,
     PRAGMA = 578,
     PREVIOUS = 579,
     PRINTER = 580,
     PRINTING = 581,
     PROCEDURE = 582,
     PROCEDURES = 583,
     PROCEED = 584,
     PROGRAM = 585,
     PROGRAM_ID = 586,
     PROGRAM_NAME = 587,
     PROGRAM_POINTER = 588,
     PROMPT = 589,
     PROCESSING = 590,
     QUOTE = 591,
     RANDOM = 592,
     RD = 593,
     READ = 594,
     READY = 595,
     RECORD = 596,
     RECORDING = 597,
     RECORDS = 598,
     RECORD_OVERFLOW = 599,
     RECURSIVE = 600,
     REDEFINES = 601,
     REEL = 602,
     REFERENCE = 603,
     RELATIVE = 604,
     RELEASE = 605,
     REMAINDER = 606,
     REMOVAL = 607,
     RENAMES = 608,
     REORG_CRITERIA = 609,
     REPLACING = 610,
     REPORT = 611,
     REPORTING = 612,
     REPORTS = 613,
     REPOSITORY = 614,
     REQUIRED = 615,
     RESERVE = 616,
     RESET = 617,
     RETURN = 618,
     RETURNING = 619,
     REVERSED = 620,
     REVERSE_FUNC = 621,
     REVERSE_VIDEO = 622,
     REWIND = 623,
     REWRITE = 624,
     RIGHT = 625,
     ROLLBACK = 626,
     ROUNDED = 627,
     RUN = 628,
     SAME = 629,
     SCREEN = 630,
     SCREEN_CONTROL = 631,
     SCROLL = 632,
     SD = 633,
     SEARCH = 634,
     SECTION = 635,
     SECURE = 636,
     SEGMENT_LIMIT = 637,
     SELECT = 638,
     SEMI_COLON = 639,
     SENTENCE = 640,
     SEPARATE = 641,
     SEQUENCE = 642,
     SEQUENTIAL = 643,
     SET = 644,
     SHARING = 645,
     SIGN = 646,
     SIGNED = 647,
     SIGNED_INT = 648,
     SIGNED_LONG = 649,
     SIGNED_SHORT = 650,
     SIZE = 651,
     SIZE_ERROR = 652,
     SORT = 653,
     SORT_MERGE = 654,
     SOURCE = 655,
     SOURCE_COMPUTER = 656,
     SPACE = 657,
     SPECIAL_NAMES = 658,
     STANDARD = 659,
     STANDARD_1 = 660,
     STANDARD_2 = 661,
     START = 662,
     STATIC = 663,
     STATUS = 664,
     STDCALL = 665,
     STOP = 666,
     STRING = 667,
     SUBSTITUTE_FUNC = 668,
     SUBSTITUTE_CASE_FUNC = 669,
     SUBTRACT = 670,
     SUCCESS = 671,
     SUM = 672,
     SUPPRESS = 673,
     SYMBOLIC = 674,
     SYNCHRONIZED = 675,
     TAB = 676,
     TALLYING = 677,
     TAPE = 678,
     TERMINATE = 679,
     TEST = 680,
     THAN = 681,
     THEN = 682,
     THRU = 683,
     TIME = 684,
     TIMEOUT = 685,
     TIMES = 686,
     TO = 687,
     TOK_FALSE = 688,
     TOK_FILE = 689,
     TOK_INITIAL = 690,
     TOK_NULL = 691,
     TOK_TRUE = 692,
     TOP = 693,
     TRAILING = 694,
     TRACE = 695,
     TRANSFORM = 696,
     TRIM_FUNCTION = 697,
     TRIML_FUNCTION = 698,
     TRIMR_FUNCTION = 699,
     TYPE = 700,
     TYPEDEF = 701,
     UNDERLINE = 702,
     UNIT = 703,
     UNLOCK = 704,
     UNSIGNED = 705,
     UNSIGNED_INT = 706,
     UNSIGNED_LONG = 707,
     UNSIGNED_SHORT = 708,
     UNSTRING = 709,
     UNTIL = 710,
     UP = 711,
     UPDATE = 712,
     UPON = 713,
     UPON_ARGUMENT_NUMBER = 714,
     UPON_COMMAND_LINE = 715,
     UPON_ENVIRONMENT_NAME = 716,
     UPON_ENVIRONMENT_VALUE = 717,
     UPPER_CASE_FUNC = 718,
     USAGE = 719,
     USE = 720,
     USING = 721,
     VALUE = 722,
     VARYING = 723,
     WAIT = 724,
     WHEN = 725,
     WHEN_COMPILED_FUNC = 726,
     WITH = 727,
     WORD = 728,
     WORDS = 729,
     WORKING_STORAGE = 730,
     WRITE = 731,
     WRITE_ONLY = 732,
     XML = 733,
     YYYYDDD = 734,
     YYYYMMDD = 735,
     ZERO = 736,
     UNARY_SIGN = 737
   };
#endif



#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif


/* Copy the second part of user declarations.  */


/* Line 264 of yacc.c  */
#line 974 "parser.c"

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int yyi)
#else
static int
YYID (yyi)
    int yyi;
#endif
{
  return yyi;
}
#endif

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef _STDLIB_H
#      define _STDLIB_H 1
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined _STDLIB_H \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef _STDLIB_H
#    define _STDLIB_H 1
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
	 || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (YYID (0))
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)				\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack_alloc, Stack, yysize);			\
	Stack = &yyptr->Stack_alloc;					\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  3
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   8462

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  496
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  784
/* YYNRULES -- Number of rules.  */
#define YYNRULES  1798
/* YYNRULES -- Number of states.  */
#define YYNSTATES  2619

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   737

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint16 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,   495,     2,
     490,   491,   484,   482,     2,   483,   488,   485,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,   494,     2,
     493,   489,   492,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,   487,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,   134,
     135,   136,   137,   138,   139,   140,   141,   142,   143,   144,
     145,   146,   147,   148,   149,   150,   151,   152,   153,   154,
     155,   156,   157,   158,   159,   160,   161,   162,   163,   164,
     165,   166,   167,   168,   169,   170,   171,   172,   173,   174,
     175,   176,   177,   178,   179,   180,   181,   182,   183,   184,
     185,   186,   187,   188,   189,   190,   191,   192,   193,   194,
     195,   196,   197,   198,   199,   200,   201,   202,   203,   204,
     205,   206,   207,   208,   209,   210,   211,   212,   213,   214,
     215,   216,   217,   218,   219,   220,   221,   222,   223,   224,
     225,   226,   227,   228,   229,   230,   231,   232,   233,   234,
     235,   236,   237,   238,   239,   240,   241,   242,   243,   244,
     245,   246,   247,   248,   249,   250,   251,   252,   253,   254,
     255,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316,   317,   318,   319,   320,   321,   322,   323,   324,
     325,   326,   327,   328,   329,   330,   331,   332,   333,   334,
     335,   336,   337,   338,   339,   340,   341,   342,   343,   344,
     345,   346,   347,   348,   349,   350,   351,   352,   353,   354,
     355,   356,   357,   358,   359,   360,   361,   362,   363,   364,
     365,   366,   367,   368,   369,   370,   371,   372,   373,   374,
     375,   376,   377,   378,   379,   380,   381,   382,   383,   384,
     385,   386,   387,   388,   389,   390,   391,   392,   393,   394,
     395,   396,   397,   398,   399,   400,   401,   402,   403,   404,
     405,   406,   407,   408,   409,   410,   411,   412,   413,   414,
     415,   416,   417,   418,   419,   420,   421,   422,   423,   424,
     425,   426,   427,   428,   429,   430,   431,   432,   433,   434,
     435,   436,   437,   438,   439,   440,   441,   442,   443,   444,
     445,   446,   447,   448,   449,   450,   451,   452,   453,   454,
     455,   456,   457,   458,   459,   460,   461,   462,   463,   464,
     465,   466,   467,   468,   469,   470,   471,   472,   473,   474,
     475,   476,   477,   478,   479,   480,   481,   486
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     4,     7,     9,    12,    13,    14,    24,
      25,    27,    30,    31,    32,    37,    38,    43,    44,    45,
      54,    55,    62,    63,    65,    66,    68,    71,    73,    75,
      77,    78,    81,    82,    86,    88,    91,    93,    95,    97,
      99,   100,   103,   107,   109,   111,   113,   116,   117,   122,
     124,   126,   129,   131,   133,   135,   137,   141,   142,   145,
     149,   152,   156,   158,   160,   163,   167,   168,   171,   175,
     178,   180,   183,   185,   187,   189,   195,   197,   199,   204,
     208,   212,   213,   216,   218,   221,   225,   229,   231,   234,
     238,   239,   242,   244,   247,   249,   251,   253,   255,   257,
     259,   261,   263,   265,   267,   269,   271,   275,   279,   282,
     284,   287,   291,   292,   298,   303,   304,   310,   312,   314,
     315,   321,   323,   325,   327,   329,   331,   333,   336,   338,
     342,   343,   348,   350,   354,   356,   358,   360,   362,   364,
     366,   368,   370,   372,   374,   376,   378,   380,   382,   386,
     388,   391,   395,   397,   400,   402,   405,   410,   412,   415,
     417,   421,   426,   431,   435,   439,   444,   448,   452,   454,
     457,   461,   463,   465,   467,   470,   473,   475,   476,   483,
     484,   487,   489,   491,   493,   495,   497,   499,   501,   503,
     505,   507,   509,   511,   513,   515,   521,   526,   531,   532,
     534,   536,   538,   539,   541,   543,   545,   547,   549,   554,
     556,   558,   560,   567,   571,   577,   578,   580,   582,   587,
     590,   593,   595,   596,   601,   607,   610,   612,   614,   617,
     621,   623,   625,   629,   632,   634,   637,   642,   647,   653,
     655,   659,   664,   669,   673,   676,   680,   683,   686,   689,
     691,   694,   697,   699,   702,   704,   707,   709,   711,   713,
     719,   720,   722,   724,   726,   732,   734,   737,   740,   741,
     744,   749,   751,   753,   755,   759,   760,   761,   766,   770,
     771,   774,   776,   778,   780,   782,   784,   786,   789,   793,
     794,   798,   801,   803,   805,   806,   811,   812,   815,   818,
     821,   823,   825,   827,   829,   831,   833,   835,   837,   839,
     845,   846,   848,   850,   855,   862,   872,   873,   877,   878,
     881,   882,   885,   889,   891,   893,   894,   896,   902,   908,
     910,   912,   914,   916,   917,   922,   928,   929,   932,   934,
     936,   938,   945,   951,   957,   962,   966,   970,   974,   975,
     982,   983,   986,   987,   990,   994,   995,   996,  1003,  1006,
    1008,  1010,  1011,  1013,  1015,  1017,  1018,  1021,  1023,  1027,
    1031,  1038,  1039,  1042,  1044,  1046,  1048,  1050,  1052,  1054,
    1056,  1058,  1060,  1062,  1064,  1066,  1068,  1070,  1072,  1075,
    1078,  1082,  1083,  1086,  1089,  1091,  1093,  1097,  1101,  1104,
    1106,  1108,  1110,  1112,  1114,  1116,  1118,  1120,  1122,  1124,
    1126,  1128,  1130,  1132,  1134,  1136,  1138,  1140,  1142,  1144,
    1146,  1149,  1152,  1154,  1157,  1160,  1162,  1165,  1168,  1170,
    1173,  1176,  1178,  1181,  1184,  1186,  1188,  1190,  1194,  1198,
    1204,  1205,  1207,  1209,  1212,  1214,  1216,  1218,  1219,  1222,
    1226,  1228,  1233,  1235,  1238,  1240,  1242,  1246,  1248,  1251,
    1253,  1256,  1259,  1260,  1262,  1264,  1268,  1270,  1272,  1273,
    1282,  1283,  1292,  1294,  1297,  1299,  1303,  1305,  1306,  1310,
    1313,  1318,  1321,  1322,  1328,  1329,  1335,  1336,  1337,  1343,
    1344,  1346,  1348,  1351,  1357,  1358,  1361,  1364,  1368,  1370,
    1372,  1376,  1380,  1383,  1384,  1386,  1387,  1389,  1391,  1393,
    1395,  1397,  1399,  1401,  1403,  1405,  1408,  1409,  1412,  1418,
    1419,  1422,  1430,  1434,  1439,  1447,  1452,  1454,  1459,  1462,
    1465,  1466,  1469,  1474,  1475,  1478,  1480,  1482,  1484,  1486,
    1488,  1490,  1492,  1494,  1496,  1498,  1500,  1502,  1504,  1506,
    1508,  1510,  1514,  1517,  1520,  1523,  1525,  1528,  1531,  1534,
    1539,  1544,  1547,  1549,  1552,  1555,  1557,  1559,  1562,  1566,
    1568,  1572,  1579,  1582,  1586,  1589,  1591,  1594,  1597,  1599,
    1602,  1603,  1605,  1607,  1612,  1615,  1616,  1618,  1620,  1621,
    1622,  1623,  1630,  1631,  1633,  1635,  1638,  1640,  1641,  1647,
    1648,  1651,  1653,  1655,  1657,  1660,  1662,  1665,  1668,  1670,
    1672,  1674,  1676,  1678,  1680,  1682,  1684,  1686,  1688,  1690,
    1692,  1697,  1703,  1709,  1713,  1717,  1719,  1721,  1723,  1725,
    1727,  1729,  1731,  1734,  1737,  1740,  1744,  1748,  1749,  1751,
    1753,  1755,  1757,  1758,  1760,  1762,  1764,  1766,  1770,  1771,
    1772,  1773,  1784,  1785,  1786,  1790,  1791,  1795,  1797,  1800,
    1805,  1806,  1809,  1812,  1815,  1816,  1818,  1819,  1823,  1827,
    1832,  1836,  1837,  1839,  1840,  1843,  1844,  1845,  1853,  1854,
    1857,  1859,  1861,  1863,  1865,  1868,  1871,  1873,  1878,  1880,
    1883,  1885,  1887,  1890,  1891,  1893,  1894,  1895,  1899,  1900,
    1902,  1903,  1906,  1909,  1911,  1913,  1915,  1917,  1919,  1921,
    1923,  1925,  1927,  1929,  1931,  1933,  1935,  1937,  1939,  1941,
    1943,  1945,  1947,  1949,  1951,  1953,  1955,  1957,  1959,  1961,
    1963,  1965,  1967,  1969,  1971,  1973,  1975,  1977,  1979,  1981,
    1983,  1985,  1987,  1989,  1991,  1993,  1995,  1997,  1999,  2001,
    2003,  2005,  2007,  2009,  2011,  2013,  2015,  2017,  2019,  2022,
    2023,  2028,  2032,  2037,  2041,  2045,  2049,  2054,  2058,  2062,
    2067,  2071,  2075,  2079,  2083,  2088,  2094,  2098,  2103,  2107,
    2111,  2114,  2117,  2120,  2125,  2130,  2135,  2136,  2138,  2140,
    2143,  2145,  2148,  2151,  2154,  2158,  2161,  2165,  2169,  2172,
    2175,  2178,  2181,  2184,  2187,  2190,  2195,  2200,  2205,  2210,
    2213,  2216,  2219,  2222,  2225,  2228,  2231,  2237,  2241,  2242,
    2244,  2245,  2250,  2255,  2261,  2268,  2269,  2272,  2273,  2275,
    2276,  2280,  2284,  2290,  2291,  2294,  2295,  2299,  2300,  2306,
    2307,  2310,  2311,  2321,  2322,  2332,  2333,  2336,  2338,  2340,
    2341,  2343,  2344,  2349,  2353,  2356,  2357,  2359,  2361,  2362,
    2364,  2365,  2367,  2368,  2372,  2374,  2377,  2380,  2386,  2387,
    2390,  2393,  2396,  2399,  2402,  2406,  2411,  2413,  2415,  2416,
    2417,  2421,  2422,  2423,  2427,  2428,  2430,  2431,  2433,  2434,
    2438,  2439,  2442,  2443,  2447,  2448,  2449,  2455,  2456,  2458,
    2461,  2462,  2464,  2467,  2470,  2471,  2474,  2475,  2479,  2480,
    2484,  2485,  2487,  2491,  2495,  2498,  2500,  2502,  2503,  2508,
    2513,  2514,  2516,  2518,  2520,  2522,  2524,  2525,  2532,  2533,
    2539,  2540,  2542,  2543,  2548,  2552,  2556,  2560,  2564,  2567,
    2569,  2572,  2574,  2576,  2579,  2582,  2585,  2588,  2591,  2594,
    2597,  2601,  2604,  2608,  2612,  2615,  2618,  2621,  2624,  2627,
    2632,  2637,  2642,  2647,  2650,  2653,  2654,  2656,  2657,  2662,
    2667,  2674,  2681,  2690,  2699,  2700,  2702,  2703,  2710,  2711,
    2713,  2714,  2718,  2720,  2723,  2726,  2732,  2737,  2738,  2741,
    2742,  2748,  2750,  2754,  2756,  2758,  2760,  2762,  2764,  2767,
    2768,  2772,  2773,  2775,  2778,  2782,  2784,  2786,  2788,  2792,
    2795,  2797,  2799,  2801,  2802,  2805,  2806,  2808,  2809,  2813,
    2814,  2815,  2819,  2821,  2824,  2826,  2828,  2829,  2832,  2833,
    2834,  2842,  2844,  2847,  2849,  2852,  2853,  2855,  2857,  2859,
    2862,  2863,  2866,  2869,  2872,  2875,  2878,  2879,  2881,  2882,
    2886,  2887,  2891,  2892,  2898,  2899,  2903,  2904,  2908,  2909,
    2912,  2913,  2917,  2918,  2923,  2925,  2927,  2930,  2934,  2939,
    2943,  2946,  2947,  2949,  2950,  2958,  2959,  2962,  2963,  2967,
    2971,  2972,  2975,  2977,  2980,  2985,  2987,  2989,  2991,  2993,
    2995,  2997,  2999,  3000,  3002,  3003,  3007,  3008,  3013,  3015,
    3017,  3019,  3021,  3024,  3026,  3028,  3030,  3031,  3035,  3037,
    3040,  3043,  3046,  3048,  3050,  3052,  3055,  3058,  3060,  3063,
    3068,  3071,  3072,  3074,  3076,  3078,  3080,  3085,  3092,  3093,
    3098,  3099,  3101,  3102,  3106,  3107,  3111,  3115,  3120,  3121,
    3126,  3131,  3138,  3139,  3141,  3142,  3146,  3147,  3154,  3156,
    3158,  3160,  3162,  3163,  3167,  3168,  3172,  3175,  3176,  3178,
    3179,  3183,  3186,  3187,  3192,  3195,  3196,  3198,  3200,  3204,
    3205,  3207,  3210,  3214,  3218,  3219,  3223,  3225,  3227,  3229,
    3233,  3241,  3242,  3247,  3249,  3252,  3253,  3255,  3256,  3268,
    3269,  3272,  3273,  3276,  3279,  3283,  3287,  3290,  3294,  3295,
    3298,  3301,  3305,  3309,  3312,  3316,  3317,  3321,  3322,  3324,
    3326,  3327,  3329,  3332,  3335,  3336,  3341,  3342,  3350,  3351,
    3353,  3354,  3362,  3363,  3366,  3370,  3371,  3373,  3375,  3376,
    3381,  3386,  3387,  3395,  3396,  3399,  3400,  3401,  3406,  3408,
    3411,  3412,  3417,  3418,  3420,  3421,  3425,  3427,  3429,  3431,
    3433,  3435,  3440,  3445,  3449,  3454,  3456,  3458,  3460,  3463,
    3467,  3469,  3472,  3476,  3480,  3481,  3485,  3486,  3494,  3495,
    3501,  3502,  3505,  3506,  3509,  3510,  3514,  3515,  3518,  3523,
    3524,  3527,  3532,  3533,  3534,  3543,  3544,  3549,  3552,  3555,
    3558,  3561,  3564,  3565,  3567,  3568,  3572,  3575,  3578,  3582,
    3583,  3585,  3588,  3591,  3592,  3601,  3603,  3606,  3608,  3612,
    3616,  3617,  3621,  3622,  3624,  3625,  3630,  3635,  3642,  3649,
    3650,  3652,  3655,  3656,  3658,  3659,  3663,  3664,  3672,  3673,
    3678,  3679,  3681,  3683,  3684,  3694,  3695,  3699,  3701,  3705,
    3708,  3711,  3714,  3718,  3719,  3723,  3724,  3728,  3729,  3733,
    3734,  3736,  3738,  3740,  3742,  3751,  3752,  3754,  3756,  3758,
    3760,  3762,  3764,  3765,  3767,  3768,  3770,  3772,  3774,  3776,
    3778,  3780,  3782,  3783,  3785,  3791,  3793,  3796,  3802,  3803,
    3812,  3813,  3816,  3817,  3822,  3826,  3830,  3832,  3834,  3835,
    3837,  3839,  3840,  3842,  3845,  3848,  3851,  3852,  3853,  3858,
    3859,  3860,  3864,  3865,  3867,  3870,  3871,  3872,  3876,  3877,
    3878,  3882,  3885,  3886,  3887,  3891,  3892,  3893,  3897,  3899,
    3901,  3904,  3905,  3909,  3910,  3914,  3916,  3918,  3921,  3922,
    3926,  3927,  3931,  3932,  3934,  3936,  3938,  3941,  3942,  3946,
    3947,  3951,  3952,  3958,  3967,  3973,  3975,  3977,  3980,  3981,
    3985,  3986,  3988,  3989,  3992,  3993,  3996,  3997,  4001,  4003,
    4005,  4006,  4009,  4011,  4014,  4016,  4018,  4020,  4022,  4024,
    4026,  4028,  4030,  4032,  4034,  4036,  4038,  4040,  4042,  4044,
    4046,  4048,  4050,  4053,  4056,  4059,  4062,  4065,  4068,  4070,
    4072,  4074,  4076,  4078,  4080,  4082,  4084,  4086,  4088,  4090,
    4092,  4094,  4096,  4099,  4101,  4103,  4106,  4108,  4111,  4113,
    4119,  4121,  4127,  4129,  4133,  4135,  4137,  4139,  4143,  4144,
    4146,  4148,  4150,  4154,  4158,  4162,  4166,  4169,  4172,  4176,
    4180,  4184,  4188,  4192,  4195,  4197,  4201,  4203,  4206,  4209,
    4211,  4213,  4215,  4218,  4220,  4222,  4225,  4227,  4228,  4231,
    4233,  4235,  4237,  4241,  4243,  4245,  4248,  4250,  4251,  4253,
    4255,  4257,  4259,  4260,  4262,  4264,  4267,  4271,  4273,  4277,
    4279,  4282,  4284,  4288,  4292,  4296,  4301,  4305,  4307,  4309,
    4311,  4313,  4317,  4321,  4325,  4327,  4329,  4331,  4333,  4335,
    4337,  4339,  4341,  4343,  4345,  4347,  4349,  4351,  4352,  4354,
    4356,  4358,  4360,  4361,  4363,  4365,  4367,  4369,  4371,  4373,
    4375,  4376,  4378,  4380,  4382,  4385,  4388,  4392,  4394,  4398,
    4400,  4404,  4407,  4411,  4416,  4422,  4424,  4426,  4428,  4431,
    4433,  4437,  4439,  4441,  4443,  4445,  4447,  4449,  4451,  4453,
    4456,  4459,  4465,  4471,  4477,  4483,  4489,  4495,  4501,  4507,
    4513,  4518,  4524,  4530,  4536,  4539,  4540,  4545,  4551,  4552,
    4556,  4557,  4559,  4561,  4565,  4569,  4571,  4575,  4577,  4581,
    4582,  4583,  4585,  4586,  4588,  4589,  4591,  4592,  4594,  4596,
    4597,  4599,  4600,  4603,  4605,  4606,  4608,  4609,  4612,  4614,
    4616,  4619,  4622,  4625,  4628,  4631,  4633,  4636,  4638,  4639,
    4641,  4642,  4644,  4645,  4647,  4648,  4650,  4651,  4653,  4654,
    4656,  4657,  4659,  4660,  4662,  4663,  4665,  4666,  4668,  4669,
    4671,  4672,  4674,  4675,  4677,  4678,  4680,  4681,  4683,  4684,
    4686,  4687,  4689,  4690,  4692,  4693,  4695,  4696,  4698,  4700,
    4701,  4703,  4704,  4706,  4707,  4709,  4711,  4712,  4714,  4715,
    4717,  4718,  4720,  4721,  4723,  4724,  4726,  4727,  4729,  4730,
    4732,  4735,  4736,  4738,  4739,  4741,  4742,  4744,  4745,  4747,
    4748,  4750,  4752,  4753,  4755,  4756,  4758,  4759,  4761,  4762,
    4764,  4767,  4768,  4770,  4771,  4773,  4774,  4776,  4777,  4779,
    4780,  4782,  4783,  4785,  4786,  4788,  4789,  4791,  4792
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int16 yyrhs[] =
{
     497,     0,    -1,    -1,   498,   499,    -1,   500,    -1,   499,
     500,    -1,    -1,    -1,   507,   518,   501,   623,   502,   770,
     503,   504,   995,    -1,    -1,   500,    -1,   503,   500,    -1,
      -1,    -1,   157,   505,  1264,   488,    -1,    -1,   153,   506,
    1264,   488,    -1,    -1,    -1,   331,   511,   508,   513,   514,
     509,   515,   511,    -1,    -1,   203,   511,   510,   513,   514,
     511,    -1,    -1,   488,    -1,    -1,   488,    -1,   512,   488,
      -1,   332,    -1,   256,    -1,   133,    -1,    -1,    28,   256,
      -1,    -1,  1250,   516,  1263,    -1,    82,    -1,    82,   517,
      -1,   187,    -1,   517,    -1,   435,    -1,   345,    -1,    -1,
     518,   519,    -1,   169,   132,   488,    -1,   520,    -1,   577,
      -1,   521,    -1,   520,   521,    -1,    -1,    93,   380,   488,
     522,    -1,   523,    -1,   524,    -1,   523,   524,    -1,   525,
      -1,   530,    -1,   543,    -1,   538,    -1,   401,   488,   526,
      -1,    -1,   528,   488,    -1,   528,   527,   488,    -1,   527,
     488,    -1,  1279,   115,   269,    -1,   529,    -1,   473,    -1,
     529,   473,    -1,   295,   488,   531,    -1,    -1,   528,   488,
      -1,   528,   532,   488,    -1,   532,   488,    -1,   533,    -1,
     532,   533,    -1,   534,    -1,   536,    -1,   537,    -1,   265,
     396,  1250,  1207,   535,    -1,    67,    -1,   474,    -1,  1263,
    1231,  1250,  1181,    -1,   382,  1250,  1207,    -1,   359,   488,
     539,    -1,    -1,   540,   488,    -1,   541,    -1,   540,   541,
      -1,   202,   542,  1249,    -1,   202,    10,   231,    -1,   473,
      -1,   542,   473,    -1,   403,   488,   544,    -1,    -1,   545,
     488,    -1,   546,    -1,   545,   546,    -1,   549,    -1,   553,
      -1,   562,    -1,   570,    -1,   567,    -1,   571,    -1,   572,
      -1,   573,    -1,   574,    -1,   575,    -1,   576,    -1,   547,
      -1,   548,   243,  1227,    -1,   548,   439,  1227,    -1,   292,
    1270,    -1,   391,    -1,   391,   234,    -1,   473,  1250,   105,
      -1,    -1,   473,  1250,  1185,   550,   551,    -1,   473,   256,
    1250,  1184,    -1,    -1,   551,   552,  1272,  1250,  1184,    -1,
     300,    -1,   298,    -1,    -1,    12,  1184,   554,  1250,   555,
      -1,   277,    -1,   405,    -1,   406,    -1,   138,    -1,   556,
      -1,   557,    -1,   556,   557,    -1,   560,    -1,   560,   428,
     560,    -1,    -1,   560,    18,   558,   559,    -1,   561,    -1,
     559,    18,   561,    -1,   256,    -1,   402,    -1,   481,    -1,
     336,    -1,   126,    -1,   215,    -1,   263,    -1,   256,    -1,
     402,    -1,   481,    -1,   336,    -1,   126,    -1,   215,    -1,
     263,    -1,   419,  1241,   563,    -1,   564,    -1,   564,   563,
      -1,   565,  1251,   566,    -1,  1184,    -1,   565,  1184,    -1,
    1207,    -1,   566,  1207,    -1,    69,  1184,  1250,   568,    -1,
     569,    -1,   568,   569,    -1,  1211,    -1,  1211,   428,  1211,
      -1,   257,  1184,  1250,  1181,    -1,   106,  1269,  1250,   256,
      -1,   116,  1250,    78,    -1,   108,  1250,  1181,    -1,   105,
     409,  1250,  1181,    -1,   376,  1250,  1181,    -1,   181,  1250,
    1181,    -1,   578,    -1,   577,   578,    -1,   228,   380,   488,
      -1,   579,    -1,   610,    -1,   580,    -1,   579,   580,    -1,
     190,   488,    -1,   581,    -1,    -1,   383,  1225,  1184,   582,
     583,   488,    -1,    -1,   583,   584,    -1,   585,    -1,   589,
      -1,   591,    -1,   592,    -1,   593,    -1,   595,    -1,   599,
      -1,   600,    -1,   602,    -1,   603,    -1,   604,    -1,   606,
      -1,   607,    -1,   608,    -1,    30,  1277,   587,   586,   588,
      -1,    30,  1277,   587,   127,    -1,    30,  1277,   587,   325,
      -1,    -1,   127,    -1,   325,    -1,   599,    -1,    -1,   187,
      -1,   136,    -1,   256,    -1,   128,    -1,  1204,    -1,     5,
    1256,  1250,   590,    -1,   388,    -1,   136,    -1,   337,    -1,
      20,  1265,  1252,  1250,   605,  1221,    -1,  1231,  1250,   473,
      -1,   594,   409,  1250,  1181,  1182,    -1,    -1,   434,    -1,
     398,    -1,   260,  1256,  1250,   596,    -1,   264,   597,    -1,
      33,   597,    -1,   183,    -1,    -1,   472,   260,   300,   598,
      -1,   472,   260,   300,   271,   598,    -1,   472,   371,    -1,
     341,    -1,   343,    -1,   253,     8,    -1,   306,  1250,   601,
      -1,   601,    -1,   222,    -1,   341,  1238,   388,    -1,  1238,
     388,    -1,   349,    -1,   253,   388,    -1,   312,  1240,  1250,
    1183,    -1,   341,   121,  1250,   405,    -1,   341,  1252,  1250,
     605,  1221,    -1,  1181,    -1,  1184,   489,  1180,    -1,  1184,
     400,  1250,  1180,    -1,   349,  1252,  1250,  1181,    -1,   361,
    1207,  1235,    -1,   361,   281,    -1,   390,  1279,   609,    -1,
      10,  1262,    -1,   281,  1262,    -1,   339,   301,    -1,   611,
      -1,   610,   611,    -1,   236,   488,    -1,   612,    -1,   613,
     488,    -1,   614,    -1,   613,   614,    -1,   615,    -1,   617,
      -1,   621,    -1,   374,   616,  1235,  1246,  1172,    -1,    -1,
     341,    -1,   398,    -1,   399,    -1,   271,  1245,  1273,  1242,
     618,    -1,   619,    -1,   618,   619,    -1,  1173,   620,    -1,
      -1,   320,  1207,    -1,    23,   622,  1259,  1172,    -1,   477,
      -1,   102,    -1,   344,    -1,   354,  1277,  1201,    -1,    -1,
      -1,   626,   624,   716,   758,    -1,   110,   132,   511,    -1,
      -1,   626,   627,    -1,   625,    -1,   628,    -1,   660,    -1,
     712,    -1,   714,    -1,   629,    -1,   628,   629,    -1,   434,
     380,   488,    -1,    -1,   632,   630,   631,    -1,   633,   662,
      -1,   189,    -1,   378,    -1,    -1,  1173,   634,   635,   488,
      -1,    -1,   635,   636,    -1,  1250,   187,    -1,  1250,   208,
      -1,   637,    -1,   639,    -1,   643,    -1,   646,    -1,   649,
      -1,   651,    -1,   657,    -1,   658,    -1,   659,    -1,    53,
    1242,  1207,   642,   638,    -1,    -1,    67,    -1,   343,    -1,
     341,  1242,  1207,  1241,    -1,   341,  1242,  1207,   432,  1207,
    1241,    -1,   341,  1250,   468,  1248,  1271,   641,   642,  1241,
     640,    -1,    -1,   122,  1259,  1181,    -1,    -1,  1247,  1207,
      -1,    -1,   432,  1207,    -1,   240,   645,   644,    -1,   404,
      -1,   299,    -1,    -1,  1229,    -1,   467,   297,   473,  1250,
     648,    -1,   467,   297,   647,  1250,   648,    -1,   191,    -1,
     216,    -1,   256,    -1,  1204,    -1,    -1,   110,   650,  1229,
    1180,    -1,   251,  1250,  1183,  1255,   652,    -1,    -1,   652,
     653,    -1,   654,    -1,   655,    -1,   656,    -1,  1279,   195,
    1255,  1237,  1183,  1255,    -1,  1255,  1237,   438,  1183,  1255,
      -1,  1255,  1237,    54,  1183,  1255,    -1,   342,  1256,  1250,
     473,    -1,    72,  1250,   473,    -1,   356,  1250,   757,    -1,
     358,  1234,   757,    -1,    -1,   475,   380,   511,   661,   662,
     511,    -1,    -1,   663,   664,    -1,    -1,  1219,   665,    -1,
     664,  1219,   665,    -1,    -1,    -1,   668,   669,   666,   674,
     667,   512,    -1,   673,   512,    -1,   473,    -1,   256,    -1,
      -1,   192,    -1,   473,    -1,   473,    -1,    -1,  1250,   208,
      -1,  1209,    -1,   245,  1258,  1202,    -1,    56,  1258,  1202,
      -1,   668,   670,    94,   671,  1236,   672,    -1,    -1,   674,
     675,    -1,   677,    -1,   678,    -1,   680,    -1,   681,    -1,
     682,    -1,   684,    -1,   685,    -1,   698,    -1,   699,    -1,
     701,    -1,   702,    -1,   705,    -1,   710,    -1,   711,    -1,
     676,    -1,  1250,   446,    -1,   346,  1202,    -1,  1250,   187,
     679,    -1,    -1,    28,   256,    -1,  1250,   208,    -1,   317,
      -1,   683,    -1,   464,  1250,   683,    -1,   464,  1250,   473,
      -1,   248,  1201,    -1,    42,    -1,    48,    -1,    83,    -1,
      85,    -1,    86,    -1,    87,    -1,    88,    -1,    89,    -1,
      90,    -1,    91,    -1,   128,    -1,   221,    -1,   311,    -1,
     319,    -1,   333,    -1,   395,    -1,   393,    -1,   394,    -1,
     453,    -1,   451,    -1,   452,    -1,    44,   392,    -1,    44,
     450,    -1,    44,    -1,    47,   392,    -1,    47,   450,    -1,
      47,    -1,    46,   392,    -1,    46,   450,    -1,    46,    -1,
      45,   392,    -1,    45,   450,    -1,    45,    -1,    43,   392,
      -1,    43,   450,    -1,    43,    -1,   274,    -1,   129,    -1,
    1270,   243,  1227,    -1,  1270,   439,  1227,    -1,   296,  1207,
     689,  1276,   686,    -1,    -1,   687,    -1,   688,    -1,   687,
     688,    -1,   690,    -1,   691,    -1,   695,    -1,    -1,   432,
    1207,    -1,   122,  1259,  1181,    -1,   693,    -1,   694,  1252,
    1250,  1180,    -1,   692,    -1,   693,   692,    -1,    29,    -1,
     123,    -1,   222,  1239,   696,    -1,   697,    -1,   696,   697,
      -1,   473,    -1,   237,  1267,    -1,   420,   700,    -1,    -1,
     244,    -1,   370,    -1,    49,  1278,   481,    -1,    35,    -1,
     707,    -1,    -1,   467,  1251,   707,   704,  1278,  1268,  1277,
     709,    -1,    -1,   467,  1251,   707,   706,  1278,  1268,  1277,
     709,    -1,   708,    -1,   707,   708,    -1,  1209,    -1,  1209,
     428,  1209,    -1,  1201,    -1,    -1,   433,  1250,  1209,    -1,
     353,  1204,    -1,   353,  1204,   428,  1204,    -1,    22,   245,
      -1,    -1,   259,   380,   488,   713,   662,    -1,    -1,   255,
     380,   488,   715,   662,    -1,    -1,    -1,   356,   380,   488,
     717,   718,    -1,    -1,   719,    -1,   720,    -1,   719,   720,
      -1,   338,   757,   721,   488,   736,    -1,    -1,   721,   722,
      -1,  1250,   208,    -1,    71,  1250,  1196,    -1,   723,    -1,
     732,    -1,    98,  1250,   724,    -1,    99,  1234,   724,    -1,
     725,   731,    -1,    -1,   193,    -1,    -1,   249,    -1,   250,
      -1,   253,    -1,   254,    -1,    75,    -1,    77,    -1,   114,
      -1,   125,    -1,    62,    -1,    98,   213,    -1,    -1,   731,
    1201,    -1,   313,   726,  1251,   735,   733,    -1,    -1,   733,
     734,    -1,   213,  1250,  1207,   194,   729,  1250,  1207,    -1,
     213,  1250,  1207,    -1,   241,   730,  1250,  1207,    -1,   241,
     729,  1250,  1207,   195,  1250,  1207,    -1,   241,   729,  1250,
    1207,    -1,  1207,    -1,  1207,   727,  1207,   728,    -1,  1207,
     727,    -1,  1207,   728,    -1,    -1,   736,   737,    -1,   668,
     669,   738,   488,    -1,    -1,   738,   739,    -1,   740,    -1,
     742,    -1,   749,    -1,   681,    -1,   682,    -1,   684,    -1,
     698,    -1,   743,    -1,   701,    -1,   754,    -1,   744,    -1,
     705,    -1,   747,    -1,   755,    -1,   685,    -1,   748,    -1,
     445,  1250,   741,    -1,   356,   213,    -1,   313,   213,    -1,
      98,   213,    -1,   125,    -1,    98,   195,    -1,   313,   195,
      -1,   356,   195,    -1,   280,   212,  1250,  1207,    -1,    76,
    1257,  1250,  1207,    -1,    76,   291,    -1,    77,    -1,    74,
     290,    -1,    74,   291,    -1,    75,    -1,   745,    -1,   744,
     745,    -1,   417,  1258,   746,    -1,  1181,    -1,   322,   470,
    1151,    -1,   468,  1201,   200,  1189,    55,  1189,    -1,   750,
     751,    -1,   253,   753,  1251,    -1,   254,  1234,    -1,   752,
      -1,   751,   752,    -1,   318,  1207,    -1,  1207,    -1,   280,
     313,    -1,    -1,   290,    -1,   291,    -1,   400,  1250,  1201,
    1226,    -1,   212,   756,    -1,    -1,   223,    -1,   473,    -1,
      -1,    -1,    -1,   375,   380,   488,   759,   760,   761,    -1,
      -1,   762,    -1,   763,    -1,   762,   763,    -1,   673,    -1,
      -1,   668,   669,   764,   765,   488,    -1,    -1,   765,   766,
      -1,    50,    -1,    51,    -1,    37,    -1,   281,    37,    -1,
      52,    -1,   177,   172,    -1,   177,   174,    -1,   214,    -1,
     262,    -1,   367,    -1,   447,    -1,   310,    -1,    32,    -1,
     421,    -1,   381,    -1,   360,    -1,   201,    -1,   463,    -1,
     261,    -1,   334,  1240,  1250,  1200,    -1,   253,  1257,  1250,
     767,  1197,    -1,    76,  1257,  1250,   768,  1197,    -1,   197,
    1250,  1198,    -1,    34,  1250,  1198,    -1,   682,    -1,   701,
      -1,   698,    -1,   684,    -1,   703,    -1,   681,    -1,   769,
      -1,   466,  1201,    -1,   200,  1190,    -1,   432,  1201,    -1,
      98,  1250,  1201,    -1,   396,  1250,  1198,    -1,    -1,   318,
      -1,   482,    -1,   267,    -1,   483,    -1,    -1,   318,    -1,
     482,    -1,   267,    -1,   483,    -1,   296,  1207,  1276,    -1,
      -1,    -1,    -1,   327,   132,   829,   773,   782,   488,   771,
     783,   772,   785,    -1,    -1,    -1,   466,   774,   776,    -1,
      -1,    64,   775,   776,    -1,   777,    -1,   776,   777,    -1,
     778,   780,   781,   473,    -1,    -1,  1239,   348,    -1,  1239,
     467,    -1,  1239,   124,    -1,    -1,   120,    -1,    -1,   396,
    1250,    32,    -1,   396,  1250,   118,    -1,   450,   396,  1250,
    1207,    -1,   396,  1250,  1207,    -1,    -1,   303,    -1,    -1,
     364,  1204,    -1,    -1,    -1,   117,   488,   784,   785,   142,
     117,   488,    -1,    -1,   785,   786,    -1,   787,    -1,   789,
      -1,   992,    -1,   790,    -1,   797,   488,    -1,     1,   488,
      -1,   488,    -1,   791,   380,   792,   488,    -1,   473,    -1,
     788,   488,    -1,   473,    -1,   473,    -1,   788,   473,    -1,
      -1,   256,    -1,    -1,    -1,   794,   795,   797,    -1,    -1,
     793,    -1,    -1,   798,   799,    -1,   797,   799,    -1,   800,
      -1,   810,    -1,   815,    -1,   819,    -1,   823,    -1,   847,
      -1,   825,    -1,   850,    -1,   858,    -1,   868,    -1,   863,
      -1,   869,    -1,   870,    -1,   874,    -1,   880,    -1,   884,
      -1,   892,    -1,   907,    -1,   912,    -1,   921,    -1,   923,
      -1,   925,    -1,   928,    -1,   931,    -1,   937,    -1,   946,
      -1,   948,    -1,   965,    -1,   967,    -1,   970,    -1,   974,
      -1,   981,    -1,   992,    -1,   996,    -1,  1004,    -1,  1006,
      -1,  1005,    -1,  1008,    -1,  1011,    -1,  1015,    -1,  1016,
      -1,  1027,    -1,  1038,    -1,  1048,    -1,  1054,    -1,  1058,
      -1,  1064,    -1,  1068,    -1,  1070,    -1,  1072,    -1,  1074,
      -1,  1077,    -1,  1088,    -1,  1101,    -1,  1142,    -1,   280,
     385,    -1,    -1,     4,   801,   802,   809,    -1,  1201,   806,
    1108,    -1,  1201,   200,   179,   239,    -1,  1201,   200,   254,
      -1,  1201,   200,    77,    -1,  1201,   200,   111,    -1,  1201,
     200,   111,   480,    -1,  1201,   200,    60,    -1,  1201,   200,
     112,    -1,  1201,   200,   112,   479,    -1,  1201,   200,    61,
      -1,  1201,   200,   113,    -1,  1201,   200,   429,    -1,  1201,
     200,    79,    -1,  1201,   200,   171,  1108,    -1,  1201,   200,
     169,  1193,  1108,    -1,  1201,   200,    26,    -1,  1201,   200,
      27,  1108,    -1,  1201,   200,  1175,    -1,  1201,   200,   473,
      -1,  1237,   804,    -1,  1237,   805,    -1,    31,  1193,    -1,
     253,  1257,  1190,  1243,    -1,    76,  1257,  1190,  1243,    -1,
     320,  1257,  1190,  1243,    -1,    -1,   807,    -1,   808,    -1,
     807,   808,    -1,   803,    -1,  1279,   100,    -1,  1279,   137,
      -1,  1279,    37,    -1,  1279,   281,    37,    -1,  1279,    52,
      -1,  1279,   177,   172,    -1,  1279,   177,   174,    -1,  1279,
     214,    -1,  1279,   262,    -1,  1279,   367,    -1,  1279,   447,
      -1,  1279,   310,    -1,  1279,   463,    -1,  1279,   261,    -1,
    1279,   197,  1250,  1198,    -1,  1279,    34,  1250,  1198,    -1,
    1279,   377,   456,  1150,    -1,  1279,   377,   134,  1150,    -1,
    1279,    32,    -1,  1279,   421,    -1,  1279,   201,    -1,  1279,
     360,    -1,  1279,   381,    -1,  1279,   457,    -1,  1279,   334,
      -1,  1279,   334,    66,  1250,   256,    -1,   430,  1092,  1198,
      -1,    -1,   143,    -1,    -1,     6,   811,   812,   814,    -1,
    1188,   432,  1168,  1116,    -1,  1188,   813,   207,  1168,  1116,
      -1,   103,  1201,   432,  1201,  1226,  1116,    -1,    -1,   432,
    1189,    -1,    -1,   144,    -1,    -1,    11,   816,   817,    -1,
     473,  1222,   818,    -1,  1152,    67,  1222,   364,  1187,    -1,
      -1,   364,  1187,    -1,    -1,    19,   820,   821,    -1,    -1,
     821,  1177,   432,   822,  1177,    -1,    -1,   329,   432,    -1,
      -1,    57,   824,   829,  1196,   830,   827,   841,   843,   845,
      -1,    -1,    63,   826,   829,  1196,   830,   833,   841,   843,
     846,    -1,    -1,   827,   828,    -1,   834,    -1,   839,    -1,
      -1,  1175,    -1,    -1,   472,   832,   831,  1253,    -1,   408,
     831,  1253,    -1,   410,  1253,    -1,    -1,    59,    -1,   410,
      -1,    -1,   408,    -1,    -1,   834,    -1,    -1,   466,   835,
     836,    -1,   837,    -1,   836,   837,    -1,   838,   299,    -1,
     838,   780,  1189,   780,   779,    -1,    -1,  1239,   348,    -1,
    1239,    96,    -1,  1239,   124,    -1,  1239,   467,    -1,   840,
    1201,    -1,   840,   230,  1201,    -1,   840,     7,  1258,  1201,
      -1,   364,    -1,   207,    -1,    -1,    -1,  1095,   842,   793,
      -1,    -1,    -1,  1096,   844,   793,    -1,    -1,   145,    -1,
      -1,   147,    -1,    -1,    58,   848,   849,    -1,    -1,   849,
    1196,    -1,    -1,    68,   851,   852,    -1,    -1,    -1,  1195,
     853,   855,   854,   857,    -1,    -1,    97,    -1,   184,   856,
      -1,    -1,  1196,    -1,   364,  1196,    -1,   472,  1196,    -1,
      -1,   207,  1201,    -1,    -1,    70,   859,   860,    -1,    -1,
     860,  1173,   861,    -1,    -1,   862,    -1,   862,  1246,   352,
      -1,  1279,   281,   368,    -1,  1279,   260,    -1,   347,    -1,
     448,    -1,    -1,    84,   864,   865,   866,    -1,  1168,   867,
    1152,  1116,    -1,    -1,   146,    -1,   489,    -1,   175,    -1,
      81,    -1,    97,    -1,    -1,   119,   871,  1173,  1265,  1136,
     873,    -1,    -1,   119,   434,   872,  1173,   873,    -1,    -1,
     148,    -1,    -1,   128,   875,   876,   879,    -1,  1189,   461,
    1109,    -1,  1189,   462,  1109,    -1,  1189,   459,  1109,    -1,
    1189,   460,  1109,    -1,   877,  1109,    -1,   878,    -1,   877,
     878,    -1,  1189,    -1,   803,    -1,   458,  1175,    -1,   458,
     473,    -1,   458,   325,    -1,   458,   105,    -1,  1279,   289,
      -1,  1279,   100,    -1,  1279,    37,    -1,  1279,   281,    37,
      -1,  1279,    52,    -1,  1279,   177,   172,    -1,  1279,   177,
     174,    -1,  1279,   214,    -1,  1279,   262,    -1,  1279,   367,
      -1,  1279,   447,    -1,  1279,   310,    -1,  1279,   197,  1250,
    1198,    -1,  1279,    34,  1250,  1198,    -1,  1279,   377,   456,
    1150,    -1,  1279,   377,   134,  1150,    -1,  1279,    50,    -1,
    1279,    51,    -1,    -1,   149,    -1,    -1,   131,   881,   882,
     883,    -1,  1189,   230,  1168,  1116,    -1,  1189,   230,  1189,
     207,  1168,  1116,    -1,  1189,    55,  1189,   207,  1168,  1116,
      -1,  1189,   230,  1189,   207,  1169,   351,  1169,  1116,    -1,
    1189,    55,  1189,   207,  1169,   351,  1169,  1116,    -1,    -1,
     150,    -1,    -1,   168,   885,   256,   829,   886,   891,    -1,
      -1,   887,    -1,    -1,   466,   888,   889,    -1,   890,    -1,
     889,   890,    -1,   838,   299,    -1,   838,   780,  1204,   780,
     779,    -1,   838,   780,    22,   780,    -1,    -1,   364,  1204,
      -1,    -1,   180,   893,   894,   896,   906,    -1,   895,    -1,
     894,    18,   895,    -1,  1152,    -1,   437,    -1,   433,    -1,
     897,    -1,   898,    -1,   897,   898,    -1,    -1,   901,   899,
     900,    -1,    -1,   793,    -1,   470,   902,    -1,   901,   470,
     902,    -1,   307,    -1,   903,    -1,   904,    -1,   903,    18,
     904,    -1,  1153,   905,    -1,    22,    -1,   437,    -1,   433,
      -1,    -1,   428,  1152,    -1,    -1,   151,    -1,    -1,   184,
     908,   909,    -1,    -1,    -1,   330,   910,   911,    -1,   316,
      -1,   316,   109,    -1,   380,    -1,   314,    -1,    -1,  1266,
    1189,    -1,    -1,    -1,   185,   913,   916,   915,   918,   914,
     920,    -1,  1189,    -1,   915,  1189,    -1,   917,    -1,   916,
     917,    -1,    -1,   273,    -1,    65,    -1,   919,    -1,   918,
     919,    -1,    -1,   458,  1175,    -1,   458,   473,    -1,   458,
     325,    -1,   458,   105,    -1,  1279,   289,    -1,    -1,   152,
      -1,    -1,   199,   922,  1186,    -1,    -1,   206,   924,  1201,
      -1,    -1,   209,  1277,   926,  1176,   927,    -1,    -1,   122,
    1259,  1196,    -1,    -1,   210,   929,   930,    -1,    -1,  1266,
    1189,    -1,    -1,   217,   932,   933,    -1,    -1,  1151,  1275,
     934,   935,    -1,     1,    -1,   154,    -1,   793,   936,    -1,
     793,   139,   154,    -1,   793,   139,   793,   936,    -1,   139,
     793,   936,    -1,   139,   154,    -1,    -1,   154,    -1,    -1,
     224,   938,  1186,   939,   940,   941,   945,    -1,    -1,  1279,
     192,    -1,    -1,    10,  1277,   467,    -1,   944,  1277,   467,
      -1,    -1,   355,   942,    -1,   943,    -1,   942,   943,    -1,
     944,  1244,    55,  1189,    -1,    13,    -1,    16,    -1,   292,
      -1,    17,    -1,   293,    -1,   274,    -1,   275,    -1,    -1,
     118,    -1,    -1,   226,   947,   731,    -1,    -1,   229,   949,
     950,   951,    -1,  1201,    -1,  1209,    -1,  1212,    -1,   952,
      -1,   951,   952,    -1,   953,    -1,   957,    -1,   962,    -1,
      -1,   422,   954,   955,    -1,   956,    -1,   955,   956,    -1,
    1193,   196,    -1,    67,   963,    -1,    10,    -1,   243,    -1,
     439,    -1,  1193,   963,    -1,   355,   958,    -1,   959,    -1,
     958,   959,    -1,    67,    55,  1193,   963,    -1,   960,   961,
      -1,    -1,    10,    -1,   243,    -1,   194,    -1,   439,    -1,
    1193,    55,  1194,   963,    -1,   101,  1233,  1193,   432,  1194,
     963,    -1,    -1,   963,  1105,   964,  1189,    -1,    -1,   435,
      -1,    -1,   266,   966,  1040,    -1,    -1,   270,   968,   969,
      -1,  1189,   432,  1186,    -1,   103,  1189,   432,  1186,    -1,
      -1,   272,   971,   972,   973,    -1,  1189,    55,  1168,  1116,
      -1,  1189,    55,  1189,   207,  1168,  1116,    -1,    -1,   155,
      -1,    -1,   302,   975,   976,    -1,    -1,   976,   977,   978,
    1172,   980,   979,    -1,   227,    -1,   308,    -1,   235,    -1,
     186,    -1,    -1,   390,  1279,   609,    -1,    -1,  1279,   281,
     368,    -1,  1279,   260,    -1,    -1,   365,    -1,    -1,   316,
     982,   983,    -1,   986,   987,    -1,    -1,   987,   984,   793,
     985,    -1,   987,   985,    -1,    -1,   156,    -1,  1177,    -1,
    1177,   428,  1177,    -1,    -1,   198,    -1,  1190,   431,    -1,
     988,   455,   989,    -1,   988,   468,   990,    -1,    -1,  1279,
     425,  1105,    -1,  1151,    -1,   184,    -1,   991,    -1,   990,
       9,   991,    -1,  1201,   200,  1189,    55,  1189,   455,  1151,
      -1,    -1,   323,   993,   256,  1200,    -1,   992,    -1,   994,
     992,    -1,    -1,   994,    -1,    -1,   339,   997,  1173,  1223,
    1265,  1000,   998,   999,  1001,  1002,  1003,    -1,    -1,   230,
    1201,    -1,    -1,   219,   260,    -1,  1279,   260,    -1,  1279,
     281,   260,    -1,  1279,   218,   260,    -1,  1279,   469,    -1,
    1279,   238,   260,    -1,    -1,   219,   260,    -1,   472,   260,
      -1,   472,   281,   260,    -1,   472,   218,   260,    -1,   472,
     469,    -1,   472,   238,   260,    -1,    -1,   239,  1250,  1201,
      -1,    -1,  1126,    -1,  1137,    -1,    -1,   158,    -1,   340,
     440,    -1,   362,   440,    -1,    -1,   350,  1007,  1170,  1103,
      -1,    -1,   363,  1009,  1173,  1265,   998,  1126,  1010,    -1,
      -1,   159,    -1,    -1,   369,  1012,  1170,  1103,  1013,  1136,
    1014,    -1,    -1,  1279,   260,    -1,  1279,   281,   260,    -1,
      -1,   160,    -1,   371,    -1,    -1,   379,  1017,  1018,  1026,
      -1,  1171,  1020,  1021,  1023,    -1,    -1,    10,  1171,  1021,
     470,  1152,  1019,   796,    -1,    -1,   468,  1201,    -1,    -1,
      -1,  1237,   142,  1022,   793,    -1,  1024,    -1,  1024,  1023,
      -1,    -1,   470,  1151,  1025,   796,    -1,    -1,   161,    -1,
      -1,   389,  1028,  1029,    -1,  1030,    -1,  1031,    -1,  1032,
      -1,  1034,    -1,  1036,    -1,   169,  1193,   432,  1193,    -1,
    1186,   432,   168,  1192,    -1,  1186,   432,  1189,    -1,  1186,
    1033,    55,  1189,    -1,   456,    -1,   134,    -1,  1035,    -1,
    1034,  1035,    -1,  1174,   432,   552,    -1,  1037,    -1,  1036,
    1037,    -1,  1186,   432,   437,    -1,  1186,   432,   433,    -1,
      -1,   398,  1039,  1040,    -1,    -1,  1204,  1042,  1044,  1045,
    1041,  1046,  1047,    -1,    -1,  1042,  1259,   694,  1252,  1043,
      -1,    -1,  1043,  1204,    -1,    -1,  1230,  1261,    -1,    -1,
    1231,  1250,  1181,    -1,    -1,   466,  1172,    -1,   227,   327,
    1250,   986,    -1,    -1,   207,  1172,    -1,   308,   327,  1250,
     986,    -1,    -1,    -1,   407,  1049,  1173,   999,  1050,  1051,
    1136,  1053,    -1,    -1,   239,  1250,  1052,  1189,    -1,  1224,
    1157,    -1,  1224,  1158,    -1,  1224,  1159,    -1,  1224,  1160,
      -1,  1224,  1161,    -1,    -1,   162,    -1,    -1,   411,  1055,
    1056,    -1,   373,  1057,    -1,   411,   256,    -1,   411,     3,
    1057,    -1,    -1,  1189,    -1,   364,  1189,    -1,   207,  1189,
      -1,    -1,   412,  1059,  1060,   230,  1201,  1062,  1121,  1063,
      -1,  1061,    -1,  1060,  1061,    -1,  1189,    -1,   120,  1239,
     396,    -1,   120,  1239,  1189,    -1,    -1,  1279,   319,  1201,
      -1,    -1,   163,    -1,    -1,   415,  1065,  1066,  1067,    -1,
    1188,   200,  1168,  1116,    -1,  1188,   200,  1189,   207,  1168,
    1116,    -1,   103,  1201,   200,  1201,  1226,  1116,    -1,    -1,
     164,    -1,   418,  1069,    -1,    -1,   326,    -1,    -1,   424,
    1071,   731,    -1,    -1,   441,  1073,  1201,   200,  1193,   432,
    1194,    -1,    -1,   449,  1075,  1173,  1076,    -1,    -1,   341,
      -1,   343,    -1,    -1,   454,  1078,  1196,  1079,  1082,  1062,
    1086,  1121,  1087,    -1,    -1,   120,  1239,  1080,    -1,  1081,
      -1,  1080,  1260,  1081,    -1,  1220,  1193,    -1,   230,  1083,
      -1,  1082,  1083,    -1,  1201,  1084,  1085,    -1,    -1,   121,
    1248,  1201,    -1,    -1,   104,  1248,  1201,    -1,    -1,   422,
    1248,  1201,    -1,    -1,   165,    -1,  1089,    -1,  1098,    -1,
    1100,    -1,   465,  1090,  1092,  1093,  1094,  1097,  1259,  1091,
      -1,    -1,   208,    -1,  1172,    -1,   227,    -1,   308,    -1,
     235,    -1,   186,    -1,    -1,     9,    -1,    -1,   404,    -1,
     182,    -1,   178,    -1,   182,    -1,   309,    -1,   285,    -1,
     287,    -1,    -1,   327,    -1,   465,  1246,   115,  1259,  1099,
      -1,  1177,    -1,    10,   328,    -1,   465,  1090,    36,   357,
    1201,    -1,    -1,   476,  1102,  1170,  1103,  1013,  1104,  1106,
    1107,    -1,    -1,   200,  1190,    -1,    -1,  1105,  1232,  1198,
    1254,    -1,  1105,  1232,  1175,    -1,  1105,  1232,   313,    -1,
      36,    -1,     9,    -1,    -1,  1131,    -1,  1137,    -1,    -1,
     166,    -1,  1111,  1113,    -1,  1111,  1113,    -1,  1111,  1113,
      -1,    -1,    -1,   182,  1112,  1115,   793,    -1,    -1,    -1,
     285,  1114,   793,    -1,    -1,  1201,    -1,  1117,  1119,    -1,
      -1,    -1,   397,  1118,   793,    -1,    -1,    -1,   288,  1120,
     793,    -1,  1122,  1124,    -1,    -1,    -1,   309,  1123,   793,
      -1,    -1,    -1,   287,  1125,   793,    -1,  1127,    -1,  1129,
      -1,  1127,  1129,    -1,    -1,   142,  1128,   793,    -1,    -1,
     283,  1130,   793,    -1,  1132,    -1,  1134,    -1,  1132,  1134,
      -1,    -1,   173,  1133,   793,    -1,    -1,   284,  1135,   793,
      -1,    -1,  1137,    -1,  1138,    -1,  1140,    -1,  1138,  1140,
      -1,    -1,   233,  1139,   793,    -1,    -1,   286,  1141,   793,
      -1,    -1,   478,  1143,  1144,  1110,  1147,    -1,   315,  1196,
    1279,  1149,  1148,  1145,  1250,   986,    -1,   206,  1201,   200,
    1196,  1146,    -1,   335,    -1,   327,    -1,   335,   327,    -1,
      -1,   104,  1248,  1201,    -1,    -1,   167,    -1,    -1,   840,
     274,    -1,    -1,   141,  1196,    -1,    -1,  1239,  1198,  1254,
      -1,  1152,    -1,  1153,    -1,    -1,  1154,  1155,    -1,  1156,
      -1,  1155,  1156,    -1,  1189,    -1,   490,    -1,   491,    -1,
     482,    -1,   483,    -1,   484,    -1,   485,    -1,   487,    -1,
      38,    -1,    40,    -1,    41,    -1,    39,    -1,  1157,    -1,
    1158,    -1,  1159,    -1,   205,    -1,   242,    -1,   278,    -1,
     282,  1157,    -1,   282,  1158,    -1,   282,  1159,    -1,   282,
     205,    -1,   282,   242,    -1,   282,   278,    -1,   282,    -1,
      21,    -1,   304,    -1,   299,    -1,   292,    -1,    13,    -1,
      14,    -1,    15,    -1,   321,    -1,   279,    -1,   416,    -1,
     188,    -1,   234,    -1,   489,    -1,   175,  1277,    -1,   176,
      -1,   492,    -1,   211,  1274,    -1,   493,    -1,   247,  1274,
      -1,   205,    -1,   211,  1274,   304,   175,  1277,    -1,   242,
      -1,   247,  1274,   304,   175,  1277,    -1,  1163,    -1,  1162,
    1165,  1163,    -1,  1166,    -1,    10,    -1,  1166,    -1,  1164,
    1165,  1166,    -1,    -1,    80,    -1,   384,    -1,  1190,    -1,
    1166,   482,  1166,    -1,  1166,   483,  1166,    -1,  1166,   484,
    1166,    -1,  1166,   485,  1166,    -1,   482,  1166,    -1,   483,
    1166,    -1,  1166,   487,  1166,    -1,   490,  1166,   491,    -1,
    1166,    38,  1166,    -1,  1166,    40,  1166,    -1,  1166,    41,
    1166,    -1,    39,  1166,    -1,   252,    -1,   252,  1228,   473,
      -1,  1169,    -1,  1168,  1169,    -1,  1189,  1226,    -1,  1204,
      -1,  1204,    -1,  1173,    -1,  1172,  1173,    -1,   473,    -1,
    1175,    -1,  1174,  1175,    -1,   268,    -1,    -1,  1176,  1177,
      -1,  1178,    -1,  1203,    -1,  1179,    -1,  1179,  1228,  1179,
      -1,   256,    -1,  1181,    -1,  1180,  1181,    -1,  1204,    -1,
      -1,  1181,    -1,  1181,    -1,   256,    -1,   473,    -1,    -1,
    1184,    -1,  1187,    -1,  1186,  1187,    -1,  1186,  1277,  1187,
      -1,  1201,    -1,     7,  1258,  1202,    -1,  1189,    -1,  1188,
    1189,    -1,  1201,    -1,   245,  1258,  1202,    -1,   245,  1258,
    1210,    -1,   245,  1258,  1212,    -1,     7,  1258,  1191,  1192,
      -1,     7,  1258,  1202,    -1,  1209,    -1,  1212,    -1,  1167,
      -1,  1201,    -1,   245,  1258,  1202,    -1,   245,  1258,  1210,
      -1,   245,  1258,  1212,    -1,  1210,    -1,  1212,    -1,  1167,
      -1,   330,    -1,   168,    -1,  1202,    -1,   256,    -1,  1201,
      -1,  1210,    -1,  1212,    -1,  1201,    -1,  1209,    -1,  1212,
      -1,    -1,  1196,    -1,  1201,    -1,  1211,    -1,  1212,    -1,
      -1,  1199,    -1,  1201,    -1,  1207,    -1,   481,    -1,  1201,
      -1,  1208,    -1,   481,    -1,    -1,   256,    -1,  1202,    -1,
    1204,    -1,  1204,  1205,    -1,  1204,  1206,    -1,  1204,  1205,
    1206,    -1,   473,    -1,   473,  1228,  1203,    -1,   473,    -1,
     473,  1228,  1204,    -1,  1204,   133,    -1,   490,  1162,   491,
      -1,   490,  1166,   494,   491,    -1,   490,  1166,   494,  1166,
     491,    -1,   256,    -1,   256,    -1,  1210,    -1,    10,  1211,
      -1,  1211,    -1,  1210,   495,  1211,    -1,   256,    -1,   402,
      -1,   481,    -1,   336,    -1,   126,    -1,   215,    -1,   263,
      -1,   436,    -1,   107,  1213,    -1,   471,  1213,    -1,   463,
     490,  1166,   491,  1213,    -1,   261,   490,  1166,   491,  1213,
      -1,   366,   490,  1166,   491,  1213,    -1,    92,   490,  1164,
     491,  1213,    -1,   413,   490,  1164,   491,  1213,    -1,   414,
     490,  1164,   491,  1213,    -1,   442,   490,  1216,   491,  1213,
      -1,   443,   490,  1166,   491,  1213,    -1,   444,   490,  1166,
     491,  1213,    -1,   294,   490,  1217,   491,    -1,   258,   490,
    1218,   491,  1213,    -1,   276,   490,  1164,   491,  1213,    -1,
     130,   490,  1164,   491,  1213,    -1,   204,  1214,    -1,    -1,
     490,  1166,   494,   491,    -1,   490,  1166,   494,  1166,   491,
      -1,    -1,   490,  1215,   491,    -1,    -1,  1164,    -1,  1166,
      -1,  1166,  1165,   243,    -1,  1166,  1165,   439,    -1,  1166,
      -1,  1166,  1165,  1166,    -1,  1166,    -1,  1166,  1165,  1181,
      -1,    -1,    -1,    10,    -1,    -1,  1230,    -1,    -1,   225,
      -1,    -1,   280,    -1,   324,    -1,    -1,   282,    -1,    -1,
     282,   303,    -1,   303,    -1,    -1,   372,    -1,    -1,   386,
    1240,    -1,   220,    -1,   297,    -1,   341,  1250,    -1,   341,
    1234,    -1,   343,  1234,    -1,   343,  1250,    -1,   472,   135,
      -1,   135,    -1,    73,   387,    -1,   387,    -1,    -1,     8,
      -1,    -1,    10,    -1,    -1,    24,    -1,    -1,    25,    -1,
      -1,    28,    -1,    -1,    31,    -1,    -1,    42,    -1,    -1,
      55,    -1,    -1,    66,    -1,    -1,    67,    -1,    -1,    95,
      -1,    -1,    78,    -1,    -1,   110,    -1,    -1,   434,    -1,
      -1,   196,    -1,    -1,   200,    -1,    -1,   220,    -1,    -1,
     231,    -1,    -1,   234,    -1,    -1,   234,    -1,    24,    -1,
      -1,   239,    -1,    -1,   255,    -1,    -1,   253,    -1,   254,
      -1,    -1,   254,    -1,    -1,   269,    -1,    -1,   290,    -1,
      -1,   297,    -1,    -1,   300,    -1,    -1,   304,    -1,    -1,
     305,    -1,   220,   305,    -1,    -1,   307,    -1,    -1,   330,
      -1,    -1,   513,    -1,    -1,   341,    -1,    -1,   364,    -1,
     207,    -1,    -1,   370,    -1,    -1,   389,    -1,    -1,   391,
      -1,    -1,   391,    -1,   391,   234,    -1,    -1,   396,    -1,
      -1,   409,    -1,    -1,   423,    -1,    -1,   426,    -1,    -1,
     427,    -1,    -1,   431,    -1,    -1,   432,    -1,    -1,   470,
      -1,    -1,   472,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   900,   900,   900,   947,   948,   953,   954,   952,   967,
     968,   969,   972,   974,   973,   997,   996,  1028,  1032,  1027,
    1091,  1090,  1140,  1141,  1144,  1145,  1146,  1150,  1151,  1152,
    1160,  1161,  1164,  1165,  1169,  1176,  1183,  1187,  1191,  1195,
    1206,  1207,  1210,  1214,  1215,  1223,  1224,  1227,  1227,  1235,
    1245,  1246,  1250,  1251,  1252,  1253,  1262,  1265,  1266,  1267,
    1268,  1272,  1279,  1283,  1284,  1292,  1295,  1296,  1297,  1298,
    1302,  1303,  1307,  1308,  1309,  1313,  1320,  1321,  1325,  1332,
    1344,  1347,  1348,  1352,  1353,  1357,  1361,  1368,  1369,  1379,
    1382,  1383,  1387,  1388,  1392,  1393,  1394,  1395,  1396,  1397,
    1398,  1399,  1400,  1401,  1402,  1403,  1410,  1415,  1422,  1422,
    1422,  1425,  1436,  1435,  1462,  1474,  1475,  1484,  1485,  1493,
    1492,  1504,  1505,  1506,  1507,  1508,  1516,  1517,  1522,  1523,
    1525,  1524,  1536,  1537,  1541,  1542,  1543,  1544,  1545,  1546,
    1547,  1551,  1552,  1553,  1554,  1555,  1556,  1557,  1564,  1571,
    1578,  1588,  1600,  1601,  1605,  1606,  1613,  1622,  1623,  1627,
    1628,  1642,  1657,  1724,  1735,  1742,  1749,  1755,  1762,  1763,
    1766,  1770,  1771,  1780,  1781,  1784,  1788,  1794,  1793,  1824,
    1825,  1829,  1831,  1832,  1833,  1834,  1836,  1837,  1838,  1839,
    1840,  1841,  1842,  1843,  1844,  1851,  1855,  1859,  1868,  1869,
    1870,  1875,  1879,  1883,  1887,  1894,  1895,  1902,  1909,  1913,
    1914,  1915,  1922,  1948,  1958,  1967,  1969,  1970,  1976,  1980,
    1981,  1982,  1985,  1986,  1987,  1991,  2003,  2004,  2009,  2015,
    2016,  2020,  2030,  2040,  2052,  2064,  2079,  2089,  2096,  2105,
    2106,  2109,  2117,  2124,  2125,  2132,  2136,  2137,  2138,  2147,
    2148,  2152,  2156,  2160,  2164,  2165,  2169,  2170,  2171,  2177,
    2202,  2203,  2204,  2205,  2211,  2218,  2219,  2223,  2226,  2227,
    2233,  2240,  2241,  2242,  2243,  2250,  2253,  2252,  2265,  2268,
    2269,  2273,  2275,  2277,  2279,  2281,  2290,  2291,  2294,  2301,
    2300,  2336,  2350,  2351,  2361,  2360,  2379,  2380,  2384,  2391,
    2398,  2399,  2400,  2401,  2402,  2403,  2404,  2405,  2406,  2413,
    2417,  2417,  2417,  2452,  2469,  2497,  2529,  2530,  2537,  2538,
    2542,  2543,  2550,  2557,  2558,  2561,  2562,  2568,  2572,  2586,
    2587,  2591,  2592,  2599,  2598,  2611,  2629,  2630,  2634,  2635,
    2636,  2640,  2647,  2654,  2664,  2699,  2716,  2720,  2732,  2732,
    2746,  2746,  2769,  2770,  2771,  2777,  2788,  2776,  2820,  2824,
    2825,  2830,  2835,  2841,  2850,  2858,  2859,  2867,  2868,  2869,
    2873,  2893,  2897,  2906,  2907,  2908,  2909,  2910,  2911,  2912,
    2913,  2914,  2915,  2916,  2917,  2918,  2919,  2920,  2925,  2935,
    2957,  2981,  2982,  2994,  3019,  3036,  3037,  3038,  3048,  3068,
    3069,  3070,  3071,  3072,  3073,  3074,  3076,  3077,  3078,  3079,
    3080,  3081,  3082,  3088,  3094,  3095,  3096,  3097,  3098,  3099,
    3100,  3101,  3102,  3103,  3104,  3105,  3106,  3107,  3108,  3109,
    3110,  3111,  3112,  3121,  3130,  3139,  3145,  3156,  3161,  3172,
    3191,  3192,  3196,  3197,  3201,  3202,  3203,  3208,  3209,  3213,
    3220,  3244,  3259,  3261,  3266,  3267,  3271,  3278,  3279,  3284,
    3294,  3301,  3304,  3305,  3306,  3313,  3320,  3345,  3347,  3347,
    3352,  3352,  3357,  3358,  3362,  3363,  3364,  3371,  3372,  3385,
    3397,  3417,  3433,  3432,  3454,  3454,  3468,  3470,  3469,  3481,
    3482,  3486,  3487,  3496,  3503,  3506,  3510,  3514,  3515,  3516,
    3523,  3524,  3528,  3531,  3531,  3532,  3532,  3532,  3533,  3533,
    3534,  3534,  3535,  3535,  3536,  3536,  3538,  3539,  3545,  3549,
    3550,  3554,  3555,  3556,  3557,  3558,  3562,  3563,  3564,  3565,
    3568,  3569,  3573,  3577,  3578,  3582,  3583,  3584,  3585,  3586,
    3587,  3588,  3589,  3590,  3591,  3592,  3593,  3594,  3595,  3596,
    3597,  3601,  3605,  3606,  3607,  3608,  3609,  3610,  3611,  3615,
    3619,  3620,  3621,  3622,  3623,  3624,  3628,  3629,  3633,  3637,
    3642,  3646,  3650,  3654,  3655,  3659,  3660,  3664,  3665,  3666,
    3669,  3669,  3669,  3672,  3676,  3679,  3679,  3682,  3689,  3690,
    3691,  3690,  3709,  3710,  3714,  3715,  3720,  3722,  3721,  3764,
    3765,  3769,  3770,  3771,  3772,  3773,  3774,  3775,  3776,  3777,
    3778,  3779,  3780,  3781,  3782,  3783,  3784,  3785,  3787,  3788,
    3789,  3813,  3832,  3851,  3855,  3859,  3860,  3861,  3862,  3863,
    3864,  3865,  3866,  3873,  3877,  3883,  3888,  3897,  3901,  3905,
    3909,  3913,  3921,  3924,  3928,  3932,  3936,  3944,  3957,  3959,
    3969,  3958,  4012,  4014,  4013,  4023,  4022,  4035,  4036,  4041,
    4048,  4050,  4054,  4062,  4070,  4073,  4083,  4085,  4093,  4101,
    4130,  4161,  4163,  4173,  4178,  4189,  4190,  4190,  4217,  4218,
    4223,  4224,  4225,  4226,  4227,  4245,  4249,  4261,  4294,  4333,
    4337,  4350,  4351,  4354,  4355,  4364,  4368,  4364,  4381,  4382,
    4386,  4386,  4404,  4408,  4409,  4410,  4411,  4412,  4413,  4414,
    4415,  4416,  4417,  4418,  4419,  4420,  4421,  4422,  4423,  4424,
    4425,  4426,  4427,  4428,  4429,  4430,  4431,  4432,  4433,  4434,
    4435,  4436,  4437,  4438,  4439,  4440,  4441,  4442,  4443,  4444,
    4445,  4446,  4447,  4448,  4449,  4450,  4451,  4452,  4453,  4454,
    4455,  4456,  4457,  4458,  4459,  4460,  4461,  4462,  4463,  4486,
    4485,  4507,  4511,  4515,  4519,  4523,  4527,  4531,  4535,  4539,
    4543,  4547,  4551,  4555,  4559,  4563,  4567,  4571,  4575,  4579,
    4587,  4588,  4589,  4594,  4599,  4600,  4604,  4605,  4609,  4610,
    4614,  4615,  4616,  4617,  4618,  4619,  4620,  4621,  4622,  4623,
    4624,  4625,  4626,  4627,  4628,  4629,  4633,  4637,  4641,  4646,
    4647,  4648,  4649,  4650,  4651,  4652,  4653,  4663,  4667,  4668,
    4677,  4677,  4683,  4698,  4712,  4728,  4729,  4733,  4734,  4743,
    4743,  4748,  4752,  4759,  4760,  4770,  4769,  4777,  4778,  4785,
    4785,  4793,  4793,  4810,  4810,  4825,  4826,  4829,  4830,  4836,
    4842,  4847,  4850,  4854,  4858,  4865,  4866,  4867,  4872,  4873,
    4877,  4878,  4883,  4882,  4895,  4896,  4901,  4908,  4915,  4917,
    4921,  4929,  4937,  4955,  4956,  4957,  4961,  4962,  4967,  4971,
    4970,  4982,  4986,  4985,  4996,  4997,  5001,  5002,  5011,  5011,
    5015,  5016,  5028,  5028,  5034,  5042,  5033,  5055,  5056,  5060,
    5068,  5069,  5070,  5071,  5075,  5076,  5084,  5084,  5088,  5089,
    5100,  5101,  5102,  5103,  5104,  5107,  5107,  5115,  5115,  5121,
    5145,  5146,  5149,  5149,  5156,  5169,  5182,  5182,  5190,  5190,
    5201,  5202,  5212,  5211,  5231,  5235,  5239,  5243,  5247,  5267,
    5268,  5273,  5274,  5275,  5276,  5277,  5278,  5279,  5280,  5281,
    5282,  5283,  5284,  5285,  5286,  5287,  5288,  5289,  5290,  5291,
    5295,  5299,  5303,  5308,  5309,  5313,  5314,  5323,  5323,  5331,
    5347,  5364,  5381,  5394,  5410,  5411,  5420,  5420,  5436,  5437,
    5442,  5441,  5451,  5452,  5457,  5464,  5480,  5491,  5492,  5517,
    5516,  5535,  5536,  5541,  5551,  5557,  5575,  5582,  5583,  5589,
    5588,  5609,  5612,  5617,  5618,  5624,  5625,  5629,  5630,  5635,
    5728,  5729,  5730,  5734,  5735,  5739,  5740,  5749,  5749,  5754,
    5756,  5755,  5763,  5780,  5797,  5815,  5836,  5837,  5849,  5856,
    5848,  5863,  5864,  5868,  5869,  5872,  5873,  5874,  5878,  5879,
    5882,  5883,  5884,  5885,  5886,  5887,  5891,  5892,  5900,  5900,
    5913,  5913,  5926,  5926,  5935,  5939,  5952,  5952,  5957,  5961,
    5973,  5973,  5979,  5978,  5986,  5990,  5995,  6000,  6007,  6014,
    6021,  6030,  6031,  6039,  6039,  6047,  6048,  6052,  6053,  6054,
    6058,  6059,  6064,  6065,  6070,  6074,  6075,  6076,  6077,  6078,
    6079,  6080,  6084,  6085,  6094,  6094,  6107,  6106,  6116,  6117,
    6118,  6122,  6123,  6127,  6128,  6129,  6135,  6135,  6140,  6141,
    6145,  6146,  6147,  6148,  6149,  6150,  6156,  6160,  6161,  6165,
    6170,  6174,  6175,  6176,  6177,  6178,  6182,  6208,  6217,  6218,
    6222,  6222,  6230,  6230,  6240,  6240,  6245,  6249,  6261,  6261,
    6267,  6282,  6302,  6303,  6312,  6312,  6317,  6318,  6336,  6337,
    6338,  6339,  6343,  6344,  6349,  6350,  6351,  6355,  6356,  6364,
    6364,  6369,  6374,  6373,  6383,  6391,  6392,  6396,  6401,  6410,
    6413,  6417,  6421,  6432,  6439,  6440,  6444,  6445,  6449,  6450,
    6455,  6469,  6468,  6488,  6489,  6491,  6491,  6498,  6498,  6523,
    6524,  6528,  6529,  6533,  6537,  6541,  6545,  6549,  6556,  6557,
    6561,  6565,  6569,  6573,  6577,  6584,  6585,  6588,  6589,  6590,
    6594,  6595,  6604,  6616,  6629,  6629,  6644,  6644,  6655,  6656,
    6665,  6665,  6681,  6682,  6686,  6693,  6694,  6703,  6716,  6716,
    6722,  6727,  6726,  6737,  6738,  6742,  6744,  6743,  6754,  6755,
    6760,  6759,  6771,  6772,  6781,  6781,  6786,  6787,  6788,  6789,
    6790,  6796,  6805,  6809,  6818,  6825,  6826,  6832,  6833,  6837,
    6846,  6847,  6851,  6855,  6867,  6867,  6873,  6872,  6895,  6898,
    6914,  6915,  6918,  6919,  6923,  6924,  6929,  6934,  6942,  6955,
    6960,  6968,  6984,  6985,  6984,  7008,  7009,  7013,  7014,  7015,
    7016,  7017,  7021,  7022,  7031,  7031,  7036,  7041,  7046,  7055,
    7056,  7057,  7058,  7067,  7067,  7076,  7077,  7081,  7082,  7083,
    7087,  7088,  7092,  7093,  7102,  7102,  7108,  7122,  7138,  7154,
    7155,  7164,  7171,  7172,  7180,  7180,  7193,  7193,  7209,  7209,
    7218,  7220,  7221,  7230,  7230,  7240,  7241,  7246,  7247,  7252,
    7259,  7260,  7265,  7272,  7273,  7277,  7278,  7282,  7283,  7287,
    7288,  7297,  7298,  7299,  7303,  7328,  7331,  7339,  7349,  7354,
    7359,  7364,  7371,  7372,  7375,  7376,  7380,  7380,  7384,  7384,
    7388,  7388,  7391,  7392,  7396,  7403,  7404,  7408,  7420,  7420,
    7435,  7436,  7441,  7444,  7448,  7452,  7459,  7460,  7463,  7464,
    7465,  7469,  7470,  7483,  7491,  7499,  7506,  7508,  7507,  7532,
    7534,  7533,  7557,  7558,  7568,  7575,  7577,  7576,  7586,  7588,
    7587,  7603,  7609,  7611,  7610,  7620,  7622,  7621,  7637,  7642,
    7647,  7657,  7656,  7668,  7667,  7683,  7688,  7693,  7703,  7702,
    7714,  7713,  7728,  7729,  7733,  7738,  7743,  7753,  7752,  7764,
    7763,  7778,  7778,  7786,  7790,  7798,  7799,  7800,  7804,  7805,
    7809,  7810,  7814,  7815,  7819,  7820,  7829,  7832,  7844,  7851,
    7858,  7858,  7868,  7870,  7874,  7876,  7877,  7879,  7880,  7881,
    7882,  7883,  7884,  7885,  7886,  7887,  7889,  7890,  7891,  7892,
    7893,  7894,  7895,  7896,  7897,  7898,  7899,  7900,  7902,  7903,
    7904,  7906,  7907,  7908,  7909,  7910,  7913,  7914,  7915,  7916,
    7917,  7920,  7920,  7920,  7921,  7921,  7922,  7922,  7923,  7923,
    7924,  7924,  7929,  7930,  7934,  7935,  7939,  7940,  7943,  7944,
    7945,  7949,  7950,  7951,  7952,  7953,  7954,  7955,  7956,  7957,
    7958,  7959,  7960,  7961,  7972,  7984,  7999,  8000,  8005,  8011,
    8017,  8037,  8041,  8057,  8079,  8080,  8085,  8091,  8092,  8097,
    8106,  8107,  8108,  8112,  8127,  8128,  8132,  8142,  8143,  8147,
    8148,  8154,  8165,  8166,  8178,  8179,  8180,  8184,  8185,  8189,
    8190,  8194,  8195,  8196,  8197,  8198,  8199,  8200,  8201,  8202,
    8206,  8207,  8208,  8209,  8210,  8211,  8212,  8216,  8217,  8221,
    8222,  8226,  8227,  8228,  8232,  8233,  8234,  8245,  8246,  8250,
    8252,  8253,  8257,  8258,  8262,  8263,  8264,  8268,  8269,  8270,
    8274,  8275,  8289,  8293,  8294,  8295,  8296,  8300,  8301,  8305,
    8306,  8307,  8311,  8320,  8324,  8336,  8348,  8359,  8360,  8370,
    8371,  8376,  8377,  8378,  8379,  8380,  8381,  8382,  8383,  8391,
    8395,  8399,  8403,  8407,  8411,  8415,  8419,  8423,  8427,  8431,
    8435,  8439,  8443,  8447,  8451,  8458,  8459,  8460,  8464,  8465,
    8469,  8470,  8475,  8482,  8489,  8499,  8506,  8516,  8523,  8537,
    8547,  8548,  8552,  8553,  8557,  8558,  8562,  8563,  8564,  8568,
    8569,  8573,  8574,  8575,  8579,  8580,  8584,  8585,  8592,  8592,
    8593,  8593,  8593,  8593,  8594,  8594,  8595,  8595,  8597,  8597,
    8598,  8598,  8599,  8599,  8600,  8600,  8601,  8601,  8602,  8602,
    8603,  8603,  8604,  8604,  8605,  8605,  8606,  8606,  8607,  8607,
    8608,  8608,  8609,  8609,  8610,  8610,  8611,  8611,  8612,  8612,
    8613,  8613,  8614,  8614,  8615,  8615,  8616,  8616,  8616,  8617,
    8617,  8618,  8618,  8619,  8619,  8619,  8620,  8620,  8621,  8621,
    8622,  8622,  8623,  8623,  8624,  8624,  8625,  8625,  8626,  8626,
    8626,  8627,  8627,  8628,  8628,  8629,  8629,  8630,  8630,  8631,
    8631,  8631,  8632,  8632,  8633,  8633,  8634,  8634,  8635,  8635,
    8635,  8636,  8636,  8637,  8637,  8638,  8638,  8639,  8639,  8640,
    8640,  8641,  8641,  8642,  8642,  8644,  8644,  8645,  8645
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "$undefined", "ABEND", "ACCEPT", "ACCESS",
  "ADD", "ADDRESS", "ADVANCING", "AFTER", "ALL", "ALLOCATE", "ALPHABET",
  "ALPHABETIC", "\"ALPHABETIC-LOWER\"", "\"ALPHABETIC-UPPER\"",
  "ALPHANUMERIC", "\"ALPHANUMERIC-EDITED\"", "ALSO", "ALTER", "ALTERNATE",
  "AND", "ANY", "APPLY", "ARE", "AREA", "\"ARGUMENT-NUMBER\"",
  "\"ARGUMENT-VALUE\"", "AS", "ASCENDING", "ASSIGN", "AT", "AUTO",
  "AUTOMATIC", "\"BACKGROUND-COLOR\"", "BASED", "BEFORE", "BELL",
  "\"B-AND\"", "\"B-NOT\"", "\"B-OR\"", "\"B-XOR\"", "BINARY",
  "\"BINARY-C-LONG\"", "\"BINARY-CHAR\"", "\"BINARY-DOUBLE\"",
  "\"BINARY-LONG\"", "\"BINARY-SHORT\"", "BIT", "BLANK", "\"BLANK-LINE\"",
  "\"BLANK-SCREEN\"", "BLINK", "BLOCK", "BOTTOM", "BY", "\"BYTE-LENGTH\"",
  "CALL", "CANCEL", "CDECL", "\"CENTURY-DATE\"", "\"CENTURY-DAY\"", "CH",
  "CHAIN", "CHAINING", "CHANGED", "CHARACTER", "CHARACTERS", "CHECKPOINT",
  "CLASS", "CLOSE", "CODE", "\"CODE-SET\"", "COLLATING", "COL", "COLS",
  "COLUMN", "COLUMNS", "COMMA", "\"COMMAND-LINE\"", "\"comma delimiter\"",
  "COMMIT", "COMMON", "COMP", "COMPUTE", "\"COMP-1\"", "\"COMP-2\"",
  "\"COMP-3\"", "\"COMP-4\"", "\"COMP-5\"", "\"COMP-6\"", "\"COMP-X\"",
  "\"FUNCTION CONCATENATE\"", "CONFIGURATION", "CONSTANT", "CONTAINS",
  "CONTENT", "CONTINUE", "CONTROL", "CONTROLS", "CONVERT", "CONVERTING",
  "\"CORE-INDEX\"", "CORRESPONDING", "COUNT", "CRT", "CURRENCY",
  "\"FUNCTION CURRENT-DATE\"", "CURSOR", "CYCLE", "DATA", "DATE", "DAY",
  "\"DAY-OF-WEEK\"", "DE", "DEBUGGING", "\"DECIMAL-POINT\"",
  "DECLARATIVES", "DEFAULT", "DELETE", "DELIMITED", "DELIMITER",
  "DEPENDING", "DESCENDING", "DESCRIPTOR", "DETAIL", "\"DIR-SEPARATOR\"",
  "DISK", "DISPLAY", "\"DISPLAY-1\"", "\"FUNCTION DISPLAY-OF\"", "DIVIDE",
  "DIVISION", "\".Identifier\"", "DOWN", "DUPLICATES", "DYNAMIC", "ECHO",
  "EBCDIC", "ELSE", "\"EMPTY-CHECK\"", "ENCODING", "END", "\"END-ACCEPT\"",
  "\"END-ADD\"", "\"END-CALL\"", "\"END-COMPUTE\"", "\"END-CHAIN\"",
  "\"END-DELETE\"", "\"END-DISPLAY\"", "\"END-DIVIDE\"",
  "\"END-EVALUATE\"", "\"END-EXHIBIT\"", "\"END FUNCTION\"", "\"END-IF\"",
  "\"END-MULTIPLY\"", "\"END-PERFORM\"", "\"END PROGRAM\"", "\"END-READ\"",
  "\"END-RETURN\"", "\"END-REWRITE\"", "\"END-SEARCH\"", "\"END-START\"",
  "\"END-STRING\"", "\"END-SUBTRACT\"", "\"END-UNSTRING\"",
  "\"END-WRITE\"", "\"END-XML\"", "ENTRY", "ENVIRONMENT",
  "\"ENVIRONMENT-NAME\"", "\"ENVIRONMENT-VALUE\"", "EOL", "EOP", "EOS",
  "EQUAL", "EQUALS", "ERASE", "ERROR", "ESCAPE", "EVALUATE",
  "\"EVENT-STATUS\"", "EXCEPTION", "EXCLUSIVE", "EXIT", "EXHIBIT",
  "EXTEND", "EXTERNAL", "FAILURE", "FD", "\"FILE-CONTROL\"", "\"FILE-ID\"",
  "FILLER", "FINAL", "FIRST", "FOOTING", "FOR", "\"FOREGROUND-COLOR\"",
  "FOREVER", "FREE", "FROM", "FULL", "FUNCTION", "\"FUNCTION-ID\"",
  "\"FUNCTION\"", "GE", "GENERATE", "GIVING", "GLOBAL", "GO", "GOBACK",
  "GREATER", "GROUP", "HEADING", "HIGHLIGHT", "\"HIGH-VALUE\"",
  "IDENTIFICATION", "IF", "IGNORE", "IGNORING", "IN", "INDEX", "INDEXED",
  "INDICATE", "INITIALIZE", "INITIALIZED", "INITIATE", "INPUT",
  "\"INPUT-OUTPUT\"", "INSPECT", "INTO", "INTRINSIC", "INVALID",
  "\"INVALID KEY\"", "IS", "\"I-O\"", "\"I-O-CONTROL\"", "JUSTIFIED",
  "KEPT", "KEY", "LABEL", "LAST", "LE", "LEADING", "LEFT", "LENGTH",
  "\"LENGTH-CHECK\"", "LESS", "LIKE", "LIMIT", "LIMITS", "LINAGE",
  "\"LINAGE-COUNTER\"", "LINE", "LINES", "LINKAGE", "\"Literal\"",
  "LOCALE", "\"FUNCTION LOCALE\"", "\"LOCAL-STORAGE\"", "LOCK",
  "\"FUNCTION LOWER-CASE\"", "LOWLIGHT", "\"LOW-VALUE\"", "MANUAL",
  "MEMORY", "MERGE", "MINUS", "\"MNEMONIC NAME\"", "MODE", "MOVE",
  "MULTIPLE", "MULTIPLY", "NAMED", "NATIONAL", "\"NATIONAL-EDITED\"",
  "\"FUNCTION NATIONAL-OF\"", "NATIVE", "NE", "NEGATIVE", "NEXT", "NO",
  "NOT", "\"NOT END\"", "\"NOT EOP\"", "\"NOT EXCEPTION\"",
  "\"NOT INVALID KEY\"", "\"NOT OVERFLOW\"", "\"NOT SIZE ERROR\"",
  "\"NO ADVANCING\"", "NUMBER", "NUMBERS", "NUMERIC", "\"NUMERIC-EDITED\"",
  "\"FUNCTION NUMVALC\"", "\"OBJECT-COMPUTER\"", "OCCURS", "OF", "OFF",
  "OMITTED", "ON", "ONLY", "OPEN", "OPTIONAL", "OR", "ORDER",
  "ORGANIZATION", "OTHER", "OUTPUT", "COB_OVERFLOW", "OVERLINE",
  "\"PACKED-DECIMAL\"", "PADDING", "PAGE", "PARAGRAPH", "PARSE", "PERFORM",
  "PICTURE", "PLUS", "POINTER", "POSITION", "POSITIVE", "PRESENT",
  "PRAGMA", "PREVIOUS", "PRINTER", "PRINTING", "PROCEDURE", "PROCEDURES",
  "PROCEED", "PROGRAM", "\"PROGRAM-ID\"", "\"Program name\"",
  "\"PROGRAM-POINTER\"", "PROMPT", "PROCESSING", "QUOTE", "RANDOM", "RD",
  "READ", "READY", "RECORD", "RECORDING", "RECORDS", "\"RECORD-OVERFLOW\"",
  "RECURSIVE", "REDEFINES", "REEL", "REFERENCE", "RELATIVE", "RELEASE",
  "REMAINDER", "REMOVAL", "RENAMES", "\"REORG-CRITERIA\"", "REPLACING",
  "REPORT", "REPORTING", "REPORTS", "REPOSITORY", "REQUIRED", "RESERVE",
  "RESET", "RETURN", "RETURNING", "REVERSED", "\"FUNCTION REVERSE\"",
  "\"REVERSE-VIDEO\"", "REWIND", "REWRITE", "RIGHT", "ROLLBACK", "ROUNDED",
  "RUN", "SAME", "SCREEN", "\"SCREEN-CONTROL\"", "SCROLL", "SD", "SEARCH",
  "SECTION", "SECURE", "\"SEGMENT-LIMIT\"", "SELECT", "\"semi-colon\"",
  "SENTENCE", "SEPARATE", "SEQUENCE", "SEQUENTIAL", "SET", "SHARING",
  "SIGN", "SIGNED", "\"SIGNED-INT\"", "\"SIGNED-LONG\"",
  "\"SIGNED-SHORT\"", "SIZE", "\"SIZE ERROR\"", "SORT", "\"SORT-MERGE\"",
  "SOURCE", "\"SOURCE-COMPUTER\"", "SPACE", "\"SPECIAL-NAMES\"",
  "STANDARD", "\"STANDARD-1\"", "\"STANDARD-2\"", "START", "STATIC",
  "STATUS", "STDCALL", "STOP", "STRING", "\"FUNCTION SUBSTITUTE\"",
  "\"FUNCTION SUBSTITUTE-CASE\"", "SUBTRACT", "SUCCESS", "SUM", "SUPPRESS",
  "SYMBOLIC", "SYNCHRONIZED", "TAB", "TALLYING", "TAPE", "TERMINATE",
  "TEST", "THAN", "THEN", "THRU", "TIME", "TIMEOUT", "TIMES", "TO",
  "\"FALSE\"", "\"FILE\"", "\"INITIAL\"", "\"NULL\"", "\"TRUE\"", "TOP",
  "TRAILING", "TRACE", "TRANSFORM", "\"FUNCTION TRIM\"",
  "\"FUNCTION TRIML\"", "\"FUNCTION TRIMR\"", "TYPE", "TYPEDEF",
  "UNDERLINE", "UNIT", "UNLOCK", "UNSIGNED", "\"UNSIGNED-INT\"",
  "\"UNSIGNED-LONG\"", "\"UNSIGNED-SHORT\"", "UNSTRING", "UNTIL", "UP",
  "UPDATE", "UPON", "\"UPON ARGUMENT-NUMBER\"", "\"UPON COMMAND-LINE\"",
  "\"UPON ENVIRONMENT-NAME\"", "\"UPON ENVIRONMENT-VALUE\"",
  "\"FUNCTION UPPER-CASE\"", "USAGE", "USE", "USING", "VALUE", "VARYING",
  "WAIT", "WHEN", "\"FUNCTION WHEN-COMPILED\"", "WITH", "\"Identifier\"",
  "WORDS", "\"WORKING-STORAGE\"", "WRITE", "\"WRITE-ONLY\"", "XML",
  "YYYYDDD", "YYYYMMDD", "ZERO", "'+'", "'-'", "'*'", "'/'", "UNARY_SIGN",
  "'^'", "'.'", "'='", "'('", "')'", "'>'", "'<'", "':'", "'&'", "$accept",
  "start", "$@1", "nested_list", "program_definition", "$@2", "$@3",
  "nested_prog", "end_program", "$@4", "$@5", "identification_division",
  "$@6", "$@7", "$@8", "_dot", "_dot_list", "program_name", "as_literal",
  "program_type", "program_type_clause", "_init_or_recurs",
  "environment_division", "environment_division_item",
  "configuration_section", "configuration_section_item", "$@9",
  "configuration_list", "configuration_paragraph",
  "source_computer_paragraph", "source_computer_entry",
  "with_debugging_mode", "computer_name", "word_list",
  "object_computer_paragraph", "object_computer_entry",
  "object_clauses_list", "object_clauses", "object_computer_memory",
  "object_char_or_word", "object_computer_sequence",
  "object_computer_segment", "repository_paragraph", "opt_repository",
  "repository_list", "repository_name", "repository_literal_list",
  "special_names_paragraph", "opt_special_names", "special_name_list",
  "special_name", "numemric_sign_clause", "numeric_sign_is",
  "mnemonic_name_clause", "$@10", "special_name_mnemonic_on_off",
  "on_or_off", "alphabet_name_clause", "$@11", "alphabet_definition",
  "alphabet_literal_list", "alphabet_literal", "@12",
  "alphabet_also_sequence", "alphabet_lits", "alphabet_also_literal",
  "symbolic_characters_clause", "symbolic_characters_list_list",
  "symbolic_characters_list", "char_list", "integer_list",
  "class_name_clause", "class_item_list", "class_item", "locale_clause",
  "currency_sign_clause", "decimal_point_clause", "cursor_clause",
  "crt_status_clause", "screen_control", "event_status",
  "input_output_section", "input_output_section_item",
  "file_control_paragraph", "file_control_paragraph_item",
  "file_control_entry", "$@13", "select_clause_sequence", "select_clause",
  "assign_clause", "_device", "_ext_clause", "assignment_name",
  "access_mode_clause", "access_mode", "alternative_record_key_clause",
  "collating_sequence_clause", "file_status_clause", "file_or_sort",
  "lock_mode_clause", "lock_mode", "lock_with", "lock_records",
  "line_advancing_clause", "organization_clause", "organization",
  "padding_character_clause", "record_delimiter_clause",
  "record_key_clause", "opt_splitk", "relative_key_clause",
  "reserve_clause", "sharing_clause", "sharing_option",
  "i_o_control_paragraph", "i_o_control_paragraph_item", "opt_i_o_control",
  "i_o_control_list", "i_o_control_clause", "same_clause", "same_option",
  "multiple_file_tape_clause", "multiple_file_list", "multiple_file",
  "multiple_file_position", "apply_clause", "apply_clause_option",
  "data_division", "$@14", "data_division_entry", "data_division_list",
  "data_division_list_item", "file_section", "file_section_item", "$@15",
  "file_description_sequence_without_type", "file_type",
  "file_description_entry", "@16", "file_description_clause_sequence",
  "file_description_clause", "block_contains_clause",
  "_records_or_characters", "record_clause", "record_depending",
  "opt_from_integer", "opt_to_integer", "label_records_clause",
  "label_option", "label_records", "value_of_clause", "file_id",
  "valueof_name", "data_records_clause", "$@17", "linage_clause",
  "linage_sequence", "linage_lines", "linage_footing", "linage_top",
  "linage_bottom", "recording_mode_clause", "code_set_clause",
  "report_clause", "working_storage_section", "$@18",
  "record_description_list", "$@19", "record_description_list_2",
  "data_description", "$@20", "$@21", "level_number", "entry_name",
  "const_name", "const_global", "lit_or_length", "constant_entry",
  "data_description_clause_sequence", "data_description_clause",
  "typdef_clause", "redefines_clause", "external_clause", "as_extname",
  "global_clause", "picture_clause", "usage_clause", "usage",
  "sign_clause", "occurs_clause", "occurs_option_list_1",
  "occurs_option_list", "occurs_option", "occurs_to_integer",
  "occurs_depending", "occurs_keys", "occurs_key_list_item",
  "occurs_key_list", "ascending_or_descending", "occurs_indexed",
  "occurs_index_list", "occurs_index", "justified_clause",
  "synchronized_clause", "left_or_right", "blank_clause", "based_clause",
  "screen_value_clause", "$@22", "value_clause", "$@23", "value_item_list",
  "value_item", "false_is", "renames_clause", "any_length_clause",
  "local_storage_section", "$@24", "linkage_section", "$@25",
  "report_section", "$@26", "opt_report_description_list",
  "report_description_list", "report_description_entry",
  "report_description_options", "report_description_option",
  "control_clause", "control_field_list", "_final", "_limit", "_line",
  "_column", "_detail", "_control_heading", "identifier_list",
  "page_limit_clause", "page_limit_options", "page_limit_option",
  "page_line_column", "report_group_description_list",
  "report_group_description_entry", "report_group_options",
  "report_group_option", "type_clause", "type_option", "next_group_clause",
  "column_clause", "sum_clause_list", "sum_clause", "ref_id_exp",
  "present_when_condition", "varying_clause", "line_clause",
  "line_keyword_clause", "report_line_integer_list", "line_or_plus",
  "_numbers", "source_clause", "group_indicate_clause", "_indicate",
  "report_name", "screen_section", "$@27", "$@28",
  "opt_screen_description_list", "screen_description_list",
  "screen_description", "$@29", "screen_options", "screen_option",
  "screen_line_plus_minus", "screen_col_plus_minus",
  "screen_occurs_clause", "procedure_division", "$@30", "$@31",
  "procedure_using_chaining", "$@32", "$@33", "procedure_param_list",
  "procedure_param", "procedure_type", "delimited_optional",
  "size_optional", "procedure_optional", "procedure_returning",
  "procedure_declaratives", "$@34", "procedure_list", "procedure",
  "section_header", "paragraph_name", "paragraph_header",
  "invalid_statement", "section_name", "opt_segment", "statement_list",
  "@35", "@36", "_statement_list", "statements", "$@37", "statement",
  "accept_statement", "$@38", "accept_body", "opt_at_line_column",
  "line_number", "column_number", "opt_accp_attr", "accp_attrs",
  "accp_attr", "end_accept", "add_statement", "$@39", "add_body", "add_to",
  "end_add", "allocate_statement", "$@40", "allocate_body",
  "allocate_returning", "alter_statement", "$@41", "alter_options",
  "_proceed_to", "call_statement", "$@42", "chain_statement", "$@43",
  "call_using_returning_list", "call_using_returning", "call_mfconv",
  "call_with", "call_conv", "call_static", "call_using", "call_using_core",
  "$@44", "call_param_list", "call_param", "call_type",
  "call_returning_core", "RETURNING_GIVING", "call_on_exception", "$@45",
  "call_not_on_exception", "$@46", "end_call", "end_chain",
  "cancel_statement", "$@47", "cancel_list", "checkpoint_statement",
  "$@48", "checkpoint_body", "$@49", "$@50", "checkpoint_options",
  "checkpoint_returning", "checkpoint_giving", "close_statement", "$@51",
  "close_list", "close_option", "reel_or_unit", "compute_statement",
  "$@52", "compute_body", "end_compute", "comp_equal", "commit_statement",
  "continue_statement", "delete_statement", "$@53", "@54", "end_delete",
  "display_statement", "$@55", "display_body", "disp_attrs", "disp_attr",
  "end_display", "divide_statement", "$@56", "divide_body", "end_divide",
  "entry_statement", "$@57", "entry_using", "entry_using_core", "$@58",
  "entry_param_list", "entry_param", "entry_returning",
  "evaluate_statement", "$@59", "evaluate_subject_list",
  "evaluate_subject", "evaluate_condition_list", "evaluate_case_list",
  "evaluate_case", "$@60", "evaluate_statement_list", "evaluate_when_list",
  "evaluate_object_list_or_other", "evaluate_object_list",
  "evaluate_object", "opt_evaluate_thru_expr", "end_evaluate",
  "exit_statement", "$@61", "exit_body", "$@62", "exit_program_returning",
  "exhibit_statement", "$@63", "$@64", "exhibit_x_list", "exhibit_options",
  "exhibit_opt", "exhibit_upon_adv", "exhibit_upon", "end_exhibit",
  "free_statement", "$@65", "generate_statement", "$@66", "goto_statement",
  "$@67", "goto_depending", "goback_statement", "$@68", "goback_option",
  "if_statement", "$@69", "if_core", "$@70", "if_else_sentence", "end_if",
  "initialize_statement", "$@71", "initialize_filler", "initialize_value",
  "initialize_replacing", "initialize_replacing_list",
  "initialize_replacing_item", "initialize_category", "initialize_default",
  "initiate_statement", "$@72", "inspect_statement", "$@73",
  "send_identifier", "inspect_list", "inspect_item", "inspect_tallying",
  "$@74", "tallying_list", "tallying_item", "inspect_replacing",
  "replacing_list", "replacing_item", "rep_keyword", "replacing_region",
  "inspect_converting", "inspect_region", "_initial", "merge_statement",
  "$@75", "move_statement", "$@76", "move_body", "multiply_statement",
  "$@77", "multiply_body", "end_multiply", "open_statement", "$@78",
  "open_list", "open_mode", "open_sharing", "open_option", "open_reversed",
  "perform_statement", "$@79", "perform_body", "$@80", "end_perform",
  "perform_procedure", "perform_option", "perform_test",
  "perform_condition", "perform_varying_list", "perform_varying",
  "pragma_statement", "$@81", "pragma_statement_list", "_pragma_statement",
  "read_statement", "$@82", "read_into", "_with_lock", "with_lock",
  "read_key", "read_handler", "end_read", "ready_statement",
  "reset_statement", "release_statement", "$@83", "return_statement",
  "$@84", "end_return", "rewrite_statement", "$@85", "write_lock",
  "end_rewrite", "rollback_statement", "search_statement", "$@86",
  "search_body", "$@87", "search_varying", "search_at_end", "$@88",
  "search_whens", "search_when", "$@89", "end_search", "set_statement",
  "$@90", "set_body", "set_environment", "set_to", "set_up_down",
  "up_or_down", "set_to_on_off_sequence", "set_to_on_off",
  "set_to_true_false_sequence", "set_to_true_false", "sort_statement",
  "$@91", "sort_body", "$@92", "sort_key_list", "opt_key_list",
  "sort_duplicates", "sort_collating", "sort_input", "sort_output",
  "start_statement", "$@93", "@94", "start_key", "start_op", "end_start",
  "stop_statement", "$@95", "stop_option", "stop_returning",
  "string_statement", "$@96", "string_item_list", "string_item",
  "opt_with_pointer", "end_string", "subtract_statement", "$@97",
  "subtract_body", "end_subtract", "suppress_statement", "_printing",
  "terminate_statement", "$@98", "transform_statement", "$@99",
  "unlock_statement", "$@100", "opt_record", "unstring_statement", "$@101",
  "unstring_delimited", "unstring_delimited_list",
  "unstring_delimited_item", "unstring_into", "unstring_into_item",
  "unstring_into_delimiter", "unstring_into_count", "unstring_tallying",
  "end_unstring", "use_statement", "use_exception", "use_global",
  "use_exception_target", "_after", "_standard", "exception_or_error",
  "exception_or_overflow", "not_exception_or_overflow", "_procedure",
  "use_debugging", "use_debugging_target", "use_reporting",
  "write_statement", "$@102", "write_from", "write_option",
  "before_or_after", "write_handler", "end_write", "on_accp_exception",
  "on_disp_exception", "on_xml_exception", "opt_on_exception", "$@103",
  "opt_not_on_exception", "$@104", "opt_on_exception_identifier",
  "on_size_error", "opt_on_size_error", "$@105", "opt_not_on_size_error",
  "$@106", "on_overflow", "opt_on_overflow", "$@107",
  "opt_not_on_overflow", "$@108", "at_end", "at_end_sentence", "$@109",
  "not_at_end_sentence", "$@110", "at_eop", "at_eop_sentence", "$@111",
  "not_at_eop_sentence", "$@112", "opt_invalid_key", "invalid_key",
  "invalid_key_sentence", "$@113", "not_invalid_key_sentence", "$@114",
  "xml_statement", "$@115", "xml_body", "xml_processing_procedure",
  "opt_count_in", "end_xml", "xml_returning", "xml_encoding",
  "_opt_scroll_lines", "condition", "expr", "partial_expr", "$@116",
  "expr_tokens", "expr_token", "eq", "gt", "lt", "ge", "le",
  "subref_exp_list", "subref_exp", "exp_list", "e_sep", "exp",
  "linage_counter", "arithmetic_x_list", "arithmetic_x", "record_name",
  "table_name", "file_name_list", "file_name", "mnemonic_name_list",
  "mnemonic_name", "procedure_name_list", "procedure_name", "label",
  "integer_label", "reference_list", "reference", "opt_reference",
  "reference_or_literal", "undefined_word", "_undefined_word",
  "target_x_list", "target_x", "x_list", "x", "arith_x", "prog_or_entry",
  "alnum_or_id", "simple_value", "simple_all_value", "null_or_id_or_lit",
  "id_or_lit_or_func", "_num_id_or_lit", "num_id_or_lit_positive",
  "num_id_or_lit", "null_or_lit", "identifier", "identifier_1",
  "qualified_label_word", "qualified_word", "subref", "refmod",
  "integer_positive", "integer", "literal", "basic_literal", "basic_value",
  "function", "func_refmod", "func_args", "list_func_args", "trim_args",
  "numvalc_args", "locale_dt_args", "not_const_word", "flag_all",
  "flag_duplicates", "flag_initialized", "flag_next", "flag_not",
  "flag_optional", "flag_rounded", "flag_separate", "in_of", "records",
  "with_dups", "coll_sequence", "_advancing", "_all", "_are", "_area",
  "_as", "_at", "_binary", "_by", "_character", "_characters", "_contains",
  "_comma", "_data", "_file", "_for", "_from", "_in", "_intrinsic", "_is",
  "_is_are", "_key", "_linkage", "_line_or_lines", "_lines", "_mode",
  "_number", "_of", "_on", "_or", "_in_order", "_other", "_program",
  "_program_name", "_record", "_returning", "_right", "_set", "_sign",
  "_sign_is", "_size", "_status", "_tape", "_than", "_then", "_times",
  "_to", "_when", "_with", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316,   317,   318,   319,   320,   321,   322,   323,   324,
     325,   326,   327,   328,   329,   330,   331,   332,   333,   334,
     335,   336,   337,   338,   339,   340,   341,   342,   343,   344,
     345,   346,   347,   348,   349,   350,   351,   352,   353,   354,
     355,   356,   357,   358,   359,   360,   361,   362,   363,   364,
     365,   366,   367,   368,   369,   370,   371,   372,   373,   374,
     375,   376,   377,   378,   379,   380,   381,   382,   383,   384,
     385,   386,   387,   388,   389,   390,   391,   392,   393,   394,
     395,   396,   397,   398,   399,   400,   401,   402,   403,   404,
     405,   406,   407,   408,   409,   410,   411,   412,   413,   414,
     415,   416,   417,   418,   419,   420,   421,   422,   423,   424,
     425,   426,   427,   428,   429,   430,   431,   432,   433,   434,
     435,   436,   437,   438,   439,   440,   441,   442,   443,   444,
     445,   446,   447,   448,   449,   450,   451,   452,   453,   454,
     455,   456,   457,   458,   459,   460,   461,   462,   463,   464,
     465,   466,   467,   468,   469,   470,   471,   472,   473,   474,
     475,   476,   477,   478,   479,   480,   481,   482,   483,   484,
     485,   486,   487,   488,   489,   490,   491,   492,   493,   494,
     495,   496,   497,   498,   499,   500,   501,   502,   503,   504,
     505,   506,   507,   508,   509,   510,   511,   512,   513,   514,
     515,   516,   517,   518,   519,   520,   521,   522,   523,   524,
     525,   526,   527,   528,   529,   530,   531,   532,   533,   534,
     535,   536,   537,   538,   539,   540,   541,   542,   543,   544,
     545,   546,   547,   548,   549,   550,   551,   552,   553,   554,
     555,   556,   557,   558,   559,   560,   561,   562,   563,   564,
     565,   566,   567,   568,   569,   570,   571,   572,   573,   574,
     575,   576,   577,   578,   579,   580,   581,   582,   583,   584,
     585,   586,   587,   588,   589,   590,   591,   592,   593,   594,
     595,   596,   597,   598,   599,   600,   601,   602,   603,   604,
     605,   606,   607,   608,   609,   610,   611,   612,   613,   614,
     615,   616,   617,   618,   619,   620,   621,   622,   623,   624,
     625,   626,   627,   628,   629,   630,   631,   632,   633,   634,
     635,   636,   637,   638,   639,   640,   641,   642,   643,   644,
     645,   646,   647,   648,   649,   650,   651,   652,   653,   654,
     655,   656,   657,   658,   659,   660,   661,   662,   663,   664,
     665,   666,   667,   668,   669,   670,   671,   672,   673,   674,
     675,   676,   677,   678,   679,   680,   681,   682,   683,   684,
     685,   686,   687,   688,   689,   690,   691,   692,   693,   694,
     695,   696,   697,   698,   699,   700,   701,   702,   703,   704,
     705,   706,   707,   708,   709,   710,   711,   712,   713,   714,
     715,   716,   717,   718,   719,   720,   721,   722,   723,   724,
     725,   726,   727,   728,   729,   730,   731,   732,   733,   734,
     735,   736,    43,    45,    42,    47,   737,    94,    46,    61,
      40,    41,    62,    60,    58,    38
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint16 yyr1[] =
{
       0,   496,   498,   497,   499,   499,   501,   502,   500,   503,
     503,   503,   504,   505,   504,   506,   504,   508,   509,   507,
     510,   507,   511,   511,   512,   512,   512,   513,   513,   513,
     514,   514,   515,   515,   516,   516,   516,   516,   517,   517,
     518,   518,   519,   519,   519,   520,   520,   522,   521,   521,
     523,   523,   524,   524,   524,   524,   525,   526,   526,   526,
     526,   527,   528,   529,   529,   530,   531,   531,   531,   531,
     532,   532,   533,   533,   533,   534,   535,   535,   536,   537,
     538,   539,   539,   540,   540,   541,   541,   542,   542,   543,
     544,   544,   545,   545,   546,   546,   546,   546,   546,   546,
     546,   546,   546,   546,   546,   546,   547,   547,   548,   548,
     548,   549,   550,   549,   549,   551,   551,   552,   552,   554,
     553,   555,   555,   555,   555,   555,   556,   556,   557,   557,
     558,   557,   559,   559,   560,   560,   560,   560,   560,   560,
     560,   561,   561,   561,   561,   561,   561,   561,   562,   563,
     563,   564,   565,   565,   566,   566,   567,   568,   568,   569,
     569,   570,   571,   572,   573,   574,   575,   576,   577,   577,
     578,   578,   578,   579,   579,   580,   580,   582,   581,   583,
     583,   584,   584,   584,   584,   584,   584,   584,   584,   584,
     584,   584,   584,   584,   584,   585,   585,   585,   586,   586,
     586,   586,   587,   587,   587,   588,   588,   588,   589,   590,
     590,   590,   591,   592,   593,   594,   594,   594,   595,   596,
     596,   596,   597,   597,   597,   597,   598,   598,   599,   600,
     600,   601,   601,   601,   601,   601,   602,   603,   604,   605,
     605,   605,   606,   607,   607,   608,   609,   609,   609,   610,
     610,   611,   611,   612,   613,   613,   614,   614,   614,   615,
     616,   616,   616,   616,   617,   618,   618,   619,   620,   620,
     621,   622,   622,   622,   622,   623,   624,   623,   625,   626,
     626,   627,   627,   627,   627,   627,   628,   628,   629,   630,
     629,   631,   632,   632,   634,   633,   635,   635,   636,   636,
     636,   636,   636,   636,   636,   636,   636,   636,   636,   637,
     638,   638,   638,   639,   639,   639,   640,   640,   641,   641,
     642,   642,   643,   644,   644,   645,   645,   646,   646,   647,
     647,   648,   648,   650,   649,   651,   652,   652,   653,   653,
     653,   654,   655,   656,   657,   658,   659,   659,   661,   660,
     663,   662,   664,   664,   664,   666,   667,   665,   665,   668,
     668,   669,   669,   669,   670,   671,   671,   672,   672,   672,
     673,   674,   674,   675,   675,   675,   675,   675,   675,   675,
     675,   675,   675,   675,   675,   675,   675,   675,   676,   677,
     678,   679,   679,   680,   681,   682,   682,   682,   682,   683,
     683,   683,   683,   683,   683,   683,   683,   683,   683,   683,
     683,   683,   683,   683,   683,   683,   683,   683,   683,   683,
     683,   683,   683,   683,   683,   683,   683,   683,   683,   683,
     683,   683,   683,   683,   683,   683,   683,   684,   684,   685,
     686,   686,   687,   687,   688,   688,   688,   689,   689,   690,
     691,   692,   693,   693,   694,   694,   695,   696,   696,   697,
     698,   699,   700,   700,   700,   701,   702,   703,   704,   703,
     706,   705,   707,   707,   708,   708,   708,   709,   709,   710,
     710,   711,   713,   712,   715,   714,   716,   717,   716,   718,
     718,   719,   719,   720,   721,   721,   722,   722,   722,   722,
     723,   723,   724,   725,   725,   726,   726,   726,   727,   727,
     728,   728,   729,   729,   730,   730,   731,   731,   732,   733,
     733,   734,   734,   734,   734,   734,   735,   735,   735,   735,
     736,   736,   737,   738,   738,   739,   739,   739,   739,   739,
     739,   739,   739,   739,   739,   739,   739,   739,   739,   739,
     739,   740,   741,   741,   741,   741,   741,   741,   741,   742,
     743,   743,   743,   743,   743,   743,   744,   744,   745,   746,
     747,   748,   749,   750,   750,   751,   751,   752,   752,   752,
     753,   753,   753,   754,   755,   756,   756,   757,   758,   759,
     760,   758,   761,   761,   762,   762,   763,   764,   763,   765,
     765,   766,   766,   766,   766,   766,   766,   766,   766,   766,
     766,   766,   766,   766,   766,   766,   766,   766,   766,   766,
     766,   766,   766,   766,   766,   766,   766,   766,   766,   766,
     766,   766,   766,   766,   766,   766,   766,   767,   767,   767,
     767,   767,   768,   768,   768,   768,   768,   769,   770,   771,
     772,   770,   773,   774,   773,   775,   773,   776,   776,   777,
     778,   778,   778,   778,   779,   779,   780,   780,   780,   780,
     780,   781,   781,   782,   782,   783,   784,   783,   785,   785,
     786,   786,   786,   786,   786,   786,   786,   787,   788,   789,
     790,   791,   791,   792,   792,   794,   795,   793,   796,   796,
     798,   797,   797,   799,   799,   799,   799,   799,   799,   799,
     799,   799,   799,   799,   799,   799,   799,   799,   799,   799,
     799,   799,   799,   799,   799,   799,   799,   799,   799,   799,
     799,   799,   799,   799,   799,   799,   799,   799,   799,   799,
     799,   799,   799,   799,   799,   799,   799,   799,   799,   799,
     799,   799,   799,   799,   799,   799,   799,   799,   799,   801,
     800,   802,   802,   802,   802,   802,   802,   802,   802,   802,
     802,   802,   802,   802,   802,   802,   802,   802,   802,   802,
     803,   803,   803,   804,   805,   805,   806,   806,   807,   807,
     808,   808,   808,   808,   808,   808,   808,   808,   808,   808,
     808,   808,   808,   808,   808,   808,   808,   808,   808,   808,
     808,   808,   808,   808,   808,   808,   808,   808,   809,   809,
     811,   810,   812,   812,   812,   813,   813,   814,   814,   816,
     815,   817,   817,   818,   818,   820,   819,   821,   821,   822,
     822,   824,   823,   826,   825,   827,   827,   828,   828,   829,
     829,   830,   830,   830,   830,   831,   831,   831,   832,   832,
     833,   833,   835,   834,   836,   836,   837,   837,   838,   838,
     838,   838,   838,   839,   839,   839,   840,   840,   841,   842,
     841,   843,   844,   843,   845,   845,   846,   846,   848,   847,
     849,   849,   851,   850,   853,   854,   852,   855,   855,   855,
     856,   856,   856,   856,   857,   857,   859,   858,   860,   860,
     861,   861,   861,   861,   861,   862,   862,   864,   863,   865,
     866,   866,   867,   867,   868,   869,   871,   870,   872,   870,
     873,   873,   875,   874,   876,   876,   876,   876,   876,   877,
     877,   878,   878,   878,   878,   878,   878,   878,   878,   878,
     878,   878,   878,   878,   878,   878,   878,   878,   878,   878,
     878,   878,   878,   878,   878,   879,   879,   881,   880,   882,
     882,   882,   882,   882,   883,   883,   885,   884,   886,   886,
     888,   887,   889,   889,   890,   890,   890,   891,   891,   893,
     892,   894,   894,   895,   895,   895,   896,   897,   897,   899,
     898,   900,   900,   901,   901,   902,   902,   903,   903,   904,
     904,   904,   904,   905,   905,   906,   906,   908,   907,   909,
     910,   909,   909,   909,   909,   909,   911,   911,   913,   914,
     912,   915,   915,   916,   916,   917,   917,   917,   918,   918,
     919,   919,   919,   919,   919,   919,   920,   920,   922,   921,
     924,   923,   926,   925,   927,   927,   929,   928,   930,   930,
     932,   931,   934,   933,   933,   935,   935,   935,   935,   935,
     935,   936,   936,   938,   937,   939,   939,   940,   940,   940,
     941,   941,   942,   942,   943,   944,   944,   944,   944,   944,
     944,   944,   945,   945,   947,   946,   949,   948,   950,   950,
     950,   951,   951,   952,   952,   952,   954,   953,   955,   955,
     956,   956,   956,   956,   956,   956,   957,   958,   958,   959,
     959,   960,   960,   960,   960,   960,   961,   962,   963,   963,
     964,   964,   966,   965,   968,   967,   969,   969,   971,   970,
     972,   972,   973,   973,   975,   974,   976,   976,   977,   977,
     977,   977,   978,   978,   979,   979,   979,   980,   980,   982,
     981,   983,   984,   983,   983,   985,   985,   986,   986,   987,
     987,   987,   987,   987,   988,   988,   989,   989,   990,   990,
     991,   993,   992,   994,   994,   995,   995,   997,   996,   998,
     998,   999,   999,   999,   999,   999,   999,   999,  1000,  1000,
    1000,  1000,  1000,  1000,  1000,  1001,  1001,  1002,  1002,  1002,
    1003,  1003,  1004,  1005,  1007,  1006,  1009,  1008,  1010,  1010,
    1012,  1011,  1013,  1013,  1013,  1014,  1014,  1015,  1017,  1016,
    1018,  1019,  1018,  1020,  1020,  1021,  1022,  1021,  1023,  1023,
    1025,  1024,  1026,  1026,  1028,  1027,  1029,  1029,  1029,  1029,
    1029,  1030,  1031,  1031,  1032,  1033,  1033,  1034,  1034,  1035,
    1036,  1036,  1037,  1037,  1039,  1038,  1041,  1040,  1042,  1042,
    1043,  1043,  1044,  1044,  1045,  1045,  1046,  1046,  1046,  1047,
    1047,  1047,  1049,  1050,  1048,  1051,  1051,  1052,  1052,  1052,
    1052,  1052,  1053,  1053,  1055,  1054,  1056,  1056,  1056,  1057,
    1057,  1057,  1057,  1059,  1058,  1060,  1060,  1061,  1061,  1061,
    1062,  1062,  1063,  1063,  1065,  1064,  1066,  1066,  1066,  1067,
    1067,  1068,  1069,  1069,  1071,  1070,  1073,  1072,  1075,  1074,
    1076,  1076,  1076,  1078,  1077,  1079,  1079,  1080,  1080,  1081,
    1082,  1082,  1083,  1084,  1084,  1085,  1085,  1086,  1086,  1087,
    1087,  1088,  1088,  1088,  1089,  1090,  1090,  1091,  1091,  1091,
    1091,  1091,  1092,  1092,  1093,  1093,  1094,  1094,  1095,  1095,
    1096,  1096,  1097,  1097,  1098,  1099,  1099,  1100,  1102,  1101,
    1103,  1103,  1104,  1104,  1104,  1104,  1105,  1105,  1106,  1106,
    1106,  1107,  1107,  1108,  1109,  1110,  1111,  1112,  1111,  1113,
    1114,  1113,  1115,  1115,  1116,  1117,  1118,  1117,  1119,  1120,
    1119,  1121,  1122,  1123,  1122,  1124,  1125,  1124,  1126,  1126,
    1126,  1128,  1127,  1130,  1129,  1131,  1131,  1131,  1133,  1132,
    1135,  1134,  1136,  1136,  1137,  1137,  1137,  1139,  1138,  1141,
    1140,  1143,  1142,  1144,  1144,  1145,  1145,  1145,  1146,  1146,
    1147,  1147,  1148,  1148,  1149,  1149,  1150,  1150,  1151,  1152,
    1154,  1153,  1155,  1155,  1156,  1156,  1156,  1156,  1156,  1156,
    1156,  1156,  1156,  1156,  1156,  1156,  1156,  1156,  1156,  1156,
    1156,  1156,  1156,  1156,  1156,  1156,  1156,  1156,  1156,  1156,
    1156,  1156,  1156,  1156,  1156,  1156,  1156,  1156,  1156,  1156,
    1156,  1157,  1157,  1157,  1158,  1158,  1159,  1159,  1160,  1160,
    1161,  1161,  1162,  1162,  1163,  1163,  1164,  1164,  1165,  1165,
    1165,  1166,  1166,  1166,  1166,  1166,  1166,  1166,  1166,  1166,
    1166,  1166,  1166,  1166,  1167,  1167,  1168,  1168,  1169,  1170,
    1171,  1172,  1172,  1173,  1174,  1174,  1175,  1176,  1176,  1177,
    1178,  1178,  1178,  1179,  1180,  1180,  1181,  1182,  1182,  1183,
    1183,  1184,  1185,  1185,  1186,  1186,  1186,  1187,  1187,  1188,
    1188,  1189,  1189,  1189,  1189,  1189,  1189,  1189,  1189,  1189,
    1190,  1190,  1190,  1190,  1190,  1190,  1190,  1191,  1191,  1192,
    1192,  1193,  1193,  1193,  1194,  1194,  1194,  1195,  1195,  1196,
    1196,  1196,  1197,  1197,  1198,  1198,  1198,  1199,  1199,  1199,
    1200,  1200,  1201,  1202,  1202,  1202,  1202,  1203,  1203,  1204,
    1204,  1204,  1205,  1206,  1206,  1207,  1208,  1209,  1209,  1210,
    1210,  1211,  1211,  1211,  1211,  1211,  1211,  1211,  1211,  1212,
    1212,  1212,  1212,  1212,  1212,  1212,  1212,  1212,  1212,  1212,
    1212,  1212,  1212,  1212,  1212,  1213,  1213,  1213,  1214,  1214,
    1215,  1215,  1216,  1216,  1216,  1217,  1217,  1218,  1218,  1219,
    1220,  1220,  1221,  1221,  1222,  1222,  1223,  1223,  1223,  1224,
    1224,  1225,  1225,  1225,  1226,  1226,  1227,  1227,  1228,  1228,
    1229,  1229,  1229,  1229,  1230,  1230,  1231,  1231,  1232,  1232,
    1233,  1233,  1234,  1234,  1235,  1235,  1236,  1236,  1237,  1237,
    1238,  1238,  1239,  1239,  1240,  1240,  1241,  1241,  1242,  1242,
    1243,  1243,  1244,  1244,  1245,  1245,  1246,  1246,  1247,  1247,
    1248,  1248,  1249,  1249,  1250,  1250,  1251,  1251,  1251,  1252,
    1252,  1253,  1253,  1254,  1254,  1254,  1255,  1255,  1256,  1256,
    1257,  1257,  1258,  1258,  1259,  1259,  1260,  1260,  1261,  1261,
    1261,  1262,  1262,  1263,  1263,  1264,  1264,  1265,  1265,  1266,
    1266,  1266,  1267,  1267,  1268,  1268,  1269,  1269,  1270,  1270,
    1270,  1271,  1271,  1272,  1272,  1273,  1273,  1274,  1274,  1275,
    1275,  1276,  1276,  1277,  1277,  1278,  1278,  1279,  1279
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     2,     1,     2,     0,     0,     9,     0,
       1,     2,     0,     0,     4,     0,     4,     0,     0,     8,
       0,     6,     0,     1,     0,     1,     2,     1,     1,     1,
       0,     2,     0,     3,     1,     2,     1,     1,     1,     1,
       0,     2,     3,     1,     1,     1,     2,     0,     4,     1,
       1,     2,     1,     1,     1,     1,     3,     0,     2,     3,
       2,     3,     1,     1,     2,     3,     0,     2,     3,     2,
       1,     2,     1,     1,     1,     5,     1,     1,     4,     3,
       3,     0,     2,     1,     2,     3,     3,     1,     2,     3,
       0,     2,     1,     2,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     3,     3,     2,     1,
       2,     3,     0,     5,     4,     0,     5,     1,     1,     0,
       5,     1,     1,     1,     1,     1,     1,     2,     1,     3,
       0,     4,     1,     3,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     3,     1,
       2,     3,     1,     2,     1,     2,     4,     1,     2,     1,
       3,     4,     4,     3,     3,     4,     3,     3,     1,     2,
       3,     1,     1,     1,     2,     2,     1,     0,     6,     0,
       2,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     5,     4,     4,     0,     1,
       1,     1,     0,     1,     1,     1,     1,     1,     4,     1,
       1,     1,     6,     3,     5,     0,     1,     1,     4,     2,
       2,     1,     0,     4,     5,     2,     1,     1,     2,     3,
       1,     1,     3,     2,     1,     2,     4,     4,     5,     1,
       3,     4,     4,     3,     2,     3,     2,     2,     2,     1,
       2,     2,     1,     2,     1,     2,     1,     1,     1,     5,
       0,     1,     1,     1,     5,     1,     2,     2,     0,     2,
       4,     1,     1,     1,     3,     0,     0,     4,     3,     0,
       2,     1,     1,     1,     1,     1,     1,     2,     3,     0,
       3,     2,     1,     1,     0,     4,     0,     2,     2,     2,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     5,
       0,     1,     1,     4,     6,     9,     0,     3,     0,     2,
       0,     2,     3,     1,     1,     0,     1,     5,     5,     1,
       1,     1,     1,     0,     4,     5,     0,     2,     1,     1,
       1,     6,     5,     5,     4,     3,     3,     3,     0,     6,
       0,     2,     0,     2,     3,     0,     0,     6,     2,     1,
       1,     0,     1,     1,     1,     0,     2,     1,     3,     3,
       6,     0,     2,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     2,     2,
       3,     0,     2,     2,     1,     1,     3,     3,     2,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       2,     2,     1,     2,     2,     1,     2,     2,     1,     2,
       2,     1,     2,     2,     1,     1,     1,     3,     3,     5,
       0,     1,     1,     2,     1,     1,     1,     0,     2,     3,
       1,     4,     1,     2,     1,     1,     3,     1,     2,     1,
       2,     2,     0,     1,     1,     3,     1,     1,     0,     8,
       0,     8,     1,     2,     1,     3,     1,     0,     3,     2,
       4,     2,     0,     5,     0,     5,     0,     0,     5,     0,
       1,     1,     2,     5,     0,     2,     2,     3,     1,     1,
       3,     3,     2,     0,     1,     0,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     2,     0,     2,     5,     0,
       2,     7,     3,     4,     7,     4,     1,     4,     2,     2,
       0,     2,     4,     0,     2,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     3,     2,     2,     2,     1,     2,     2,     2,     4,
       4,     2,     1,     2,     2,     1,     1,     2,     3,     1,
       3,     6,     2,     3,     2,     1,     2,     2,     1,     2,
       0,     1,     1,     4,     2,     0,     1,     1,     0,     0,
       0,     6,     0,     1,     1,     2,     1,     0,     5,     0,
       2,     1,     1,     1,     2,     1,     2,     2,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       4,     5,     5,     3,     3,     1,     1,     1,     1,     1,
       1,     1,     2,     2,     2,     3,     3,     0,     1,     1,
       1,     1,     0,     1,     1,     1,     1,     3,     0,     0,
       0,    10,     0,     0,     3,     0,     3,     1,     2,     4,
       0,     2,     2,     2,     0,     1,     0,     3,     3,     4,
       3,     0,     1,     0,     2,     0,     0,     7,     0,     2,
       1,     1,     1,     1,     2,     2,     1,     4,     1,     2,
       1,     1,     2,     0,     1,     0,     0,     3,     0,     1,
       0,     2,     2,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     2,     0,
       4,     3,     4,     3,     3,     3,     4,     3,     3,     4,
       3,     3,     3,     3,     4,     5,     3,     4,     3,     3,
       2,     2,     2,     4,     4,     4,     0,     1,     1,     2,
       1,     2,     2,     2,     3,     2,     3,     3,     2,     2,
       2,     2,     2,     2,     2,     4,     4,     4,     4,     2,
       2,     2,     2,     2,     2,     2,     5,     3,     0,     1,
       0,     4,     4,     5,     6,     0,     2,     0,     1,     0,
       3,     3,     5,     0,     2,     0,     3,     0,     5,     0,
       2,     0,     9,     0,     9,     0,     2,     1,     1,     0,
       1,     0,     4,     3,     2,     0,     1,     1,     0,     1,
       0,     1,     0,     3,     1,     2,     2,     5,     0,     2,
       2,     2,     2,     2,     3,     4,     1,     1,     0,     0,
       3,     0,     0,     3,     0,     1,     0,     1,     0,     3,
       0,     2,     0,     3,     0,     0,     5,     0,     1,     2,
       0,     1,     2,     2,     0,     2,     0,     3,     0,     3,
       0,     1,     3,     3,     2,     1,     1,     0,     4,     4,
       0,     1,     1,     1,     1,     1,     0,     6,     0,     5,
       0,     1,     0,     4,     3,     3,     3,     3,     2,     1,
       2,     1,     1,     2,     2,     2,     2,     2,     2,     2,
       3,     2,     3,     3,     2,     2,     2,     2,     2,     4,
       4,     4,     4,     2,     2,     0,     1,     0,     4,     4,
       6,     6,     8,     8,     0,     1,     0,     6,     0,     1,
       0,     3,     1,     2,     2,     5,     4,     0,     2,     0,
       5,     1,     3,     1,     1,     1,     1,     1,     2,     0,
       3,     0,     1,     2,     3,     1,     1,     1,     3,     2,
       1,     1,     1,     0,     2,     0,     1,     0,     3,     0,
       0,     3,     1,     2,     1,     1,     0,     2,     0,     0,
       7,     1,     2,     1,     2,     0,     1,     1,     1,     2,
       0,     2,     2,     2,     2,     2,     0,     1,     0,     3,
       0,     3,     0,     5,     0,     3,     0,     3,     0,     2,
       0,     3,     0,     4,     1,     1,     2,     3,     4,     3,
       2,     0,     1,     0,     7,     0,     2,     0,     3,     3,
       0,     2,     1,     2,     4,     1,     1,     1,     1,     1,
       1,     1,     0,     1,     0,     3,     0,     4,     1,     1,
       1,     1,     2,     1,     1,     1,     0,     3,     1,     2,
       2,     2,     1,     1,     1,     2,     2,     1,     2,     4,
       2,     0,     1,     1,     1,     1,     4,     6,     0,     4,
       0,     1,     0,     3,     0,     3,     3,     4,     0,     4,
       4,     6,     0,     1,     0,     3,     0,     6,     1,     1,
       1,     1,     0,     3,     0,     3,     2,     0,     1,     0,
       3,     2,     0,     4,     2,     0,     1,     1,     3,     0,
       1,     2,     3,     3,     0,     3,     1,     1,     1,     3,
       7,     0,     4,     1,     2,     0,     1,     0,    11,     0,
       2,     0,     2,     2,     3,     3,     2,     3,     0,     2,
       2,     3,     3,     2,     3,     0,     3,     0,     1,     1,
       0,     1,     2,     2,     0,     4,     0,     7,     0,     1,
       0,     7,     0,     2,     3,     0,     1,     1,     0,     4,
       4,     0,     7,     0,     2,     0,     0,     4,     1,     2,
       0,     4,     0,     1,     0,     3,     1,     1,     1,     1,
       1,     4,     4,     3,     4,     1,     1,     1,     2,     3,
       1,     2,     3,     3,     0,     3,     0,     7,     0,     5,
       0,     2,     0,     2,     0,     3,     0,     2,     4,     0,
       2,     4,     0,     0,     8,     0,     4,     2,     2,     2,
       2,     2,     0,     1,     0,     3,     2,     2,     3,     0,
       1,     2,     2,     0,     8,     1,     2,     1,     3,     3,
       0,     3,     0,     1,     0,     4,     4,     6,     6,     0,
       1,     2,     0,     1,     0,     3,     0,     7,     0,     4,
       0,     1,     1,     0,     9,     0,     3,     1,     3,     2,
       2,     2,     3,     0,     3,     0,     3,     0,     3,     0,
       1,     1,     1,     1,     8,     0,     1,     1,     1,     1,
       1,     1,     0,     1,     0,     1,     1,     1,     1,     1,
       1,     1,     0,     1,     5,     1,     2,     5,     0,     8,
       0,     2,     0,     4,     3,     3,     1,     1,     0,     1,
       1,     0,     1,     2,     2,     2,     0,     0,     4,     0,
       0,     3,     0,     1,     2,     0,     0,     3,     0,     0,
       3,     2,     0,     0,     3,     0,     0,     3,     1,     1,
       2,     0,     3,     0,     3,     1,     1,     2,     0,     3,
       0,     3,     0,     1,     1,     1,     2,     0,     3,     0,
       3,     0,     5,     8,     5,     1,     1,     2,     0,     3,
       0,     1,     0,     2,     0,     2,     0,     3,     1,     1,
       0,     2,     1,     2,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     2,     2,     2,     2,     2,     2,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     2,     1,     1,     2,     1,     2,     1,     5,
       1,     5,     1,     3,     1,     1,     1,     3,     0,     1,
       1,     1,     3,     3,     3,     3,     2,     2,     3,     3,
       3,     3,     3,     2,     1,     3,     1,     2,     2,     1,
       1,     1,     2,     1,     1,     2,     1,     0,     2,     1,
       1,     1,     3,     1,     1,     2,     1,     0,     1,     1,
       1,     1,     0,     1,     1,     2,     3,     1,     3,     1,
       2,     1,     3,     3,     3,     4,     3,     1,     1,     1,
       1,     3,     3,     3,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     0,     1,     1,
       1,     1,     0,     1,     1,     1,     1,     1,     1,     1,
       0,     1,     1,     1,     2,     2,     3,     1,     3,     1,
       3,     2,     3,     4,     5,     1,     1,     1,     2,     1,
       3,     1,     1,     1,     1,     1,     1,     1,     1,     2,
       2,     5,     5,     5,     5,     5,     5,     5,     5,     5,
       4,     5,     5,     5,     2,     0,     4,     5,     0,     3,
       0,     1,     1,     3,     3,     1,     3,     1,     3,     0,
       0,     1,     0,     1,     0,     1,     0,     1,     1,     0,
       1,     0,     2,     1,     0,     1,     0,     2,     1,     1,
       2,     2,     2,     2,     2,     1,     2,     1,     0,     1,
       0,     1,     0,     1,     0,     1,     0,     1,     0,     1,
       0,     1,     0,     1,     0,     1,     0,     1,     0,     1,
       0,     1,     0,     1,     0,     1,     0,     1,     0,     1,
       0,     1,     0,     1,     0,     1,     0,     1,     1,     0,
       1,     0,     1,     0,     1,     1,     0,     1,     0,     1,
       0,     1,     0,     1,     0,     1,     0,     1,     0,     1,
       2,     0,     1,     0,     1,     0,     1,     0,     1,     0,
       1,     1,     0,     1,     0,     1,     0,     1,     0,     1,
       2,     0,     1,     0,     1,     0,     1,     0,     1,     0,
       1,     0,     1,     0,     1,     0,     1,     0,     1
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint16 yydefact[] =
{
       2,     0,     0,     1,    22,    22,     3,     4,    40,    23,
      20,    17,     5,     6,     0,     0,     0,     0,     0,     0,
       0,     0,  1724,     0,     0,   260,  1681,     0,     0,   279,
      41,    43,    45,    49,    50,    52,    53,    55,    54,    44,
     168,   171,   173,   176,   172,   249,   252,     0,   254,   256,
     257,   258,    29,    28,    27,    30,    30,   272,   273,  1793,
     271,  1754,     0,     0,   175,     0,   251,  1725,  1785,    66,
      81,   261,   262,   263,  1704,     0,  1683,     0,    57,    90,
       7,   276,    46,    51,   169,   174,   250,   253,   255,     0,
      22,    18,  1794,     0,  1755,     0,    47,    42,   170,  1786,
    1718,     0,  1764,  1734,    63,  1763,    62,    65,  1763,    70,
      72,    73,    74,     0,     0,    80,     0,    83,  1705,  1726,
    1682,  1561,   177,  1798,    56,     0,  1797,     0,     0,     0,
       0,  1776,  1734,  1734,  1734,     0,  1778,  1734,   109,  1716,
    1734,    89,     0,    92,   105,     0,    94,    95,    96,    98,
      97,    99,   100,   101,   102,   103,   104,   648,     0,   292,
       0,     0,   293,     0,     0,   486,   281,   280,   282,   286,
     289,   283,   284,   285,    31,    21,    32,  1619,   274,  1612,
    1613,  1543,   270,  1541,    48,  1719,     0,  1734,  1735,     0,
      67,  1763,    64,    69,    71,     0,  1697,  1734,     0,    87,
    1732,    82,    84,  1727,     0,   179,    60,    58,     0,     0,
     119,  1734,  1734,  1777,  1734,     0,     0,     0,  1734,  1779,
     108,     0,   110,  1717,     0,  1734,  1562,    91,    93,  1686,
    1686,     0,     9,    22,     0,     0,     0,    22,     0,   588,
     287,     0,    22,     0,  1688,  1689,     0,  1621,     0,  1614,
    1615,  1542,   264,   265,   268,     0,  1625,    79,    68,  1696,
       0,    86,  1733,    88,    85,   259,   215,    59,    61,  1734,
       0,     0,     0,   164,  1556,   163,   167,     0,  1780,   166,
     148,   149,  1736,   152,     0,   111,  1563,   112,  1714,   106,
     107,   849,    10,    12,   278,   484,   482,   288,   348,     0,
       0,   277,   290,   350,   294,    19,    34,    36,    39,    38,
    1763,    37,  1620,  1515,     0,     0,  1655,  1635,     0,  1658,
    1636,  1752,  1534,  1631,     0,     0,  1637,     0,     0,  1634,
       0,  1632,     0,     0,  1638,     0,     0,     0,     0,  1655,
    1633,     0,     0,     0,  1518,  1512,  1514,  1586,  1521,  1580,
    1584,  1629,  1585,     0,  1616,   266,     0,   267,     0,    78,
    1748,  1767,  1793,  1711,   231,     0,  1748,  1734,  1714,  1739,
     234,     0,  1797,   217,   216,   178,   180,   181,   182,   183,
     184,   185,     0,   186,   187,   188,   230,   189,   190,   191,
     192,   193,   194,  1734,     0,     0,   156,   157,   159,   165,
     162,   161,   150,  1738,  1737,   153,     0,   114,   115,  1715,
    1687,  1546,   652,   850,    15,    13,    11,  1185,   350,   350,
     350,   487,     0,   291,   352,   296,    35,    33,  1533,     0,
       0,  1639,     0,  1660,  1654,  1753,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1640,
    1526,  1527,     0,  1519,  1520,  1622,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   269,    76,
      77,    75,  1749,  1734,  1768,  1739,   202,   228,   235,  1734,
    1710,  1734,  1734,  1740,     0,  1734,  1734,   244,  1704,     0,
    1734,     0,   233,   138,   124,   139,   134,   140,   121,   137,
     135,   122,   123,   136,   120,   125,   126,   128,   158,     0,
     151,   154,   113,   655,   653,   673,  1765,  1765,  1181,  1183,
    1186,     8,   485,   483,    22,   489,   589,   351,     0,  1734,
    1518,  1516,     0,  1518,  1518,     0,  1581,  1582,  1583,  1535,
    1518,     0,     0,  1518,  1518,     0,     0,  1518,  1518,  1518,
       0,     0,     0,     0,  1529,  1513,  1514,  1530,  1531,  1532,
    1522,  1523,  1524,  1525,  1528,  1623,     0,  1630,     0,  1734,
     204,   203,   198,     0,     0,  1710,   234,   229,     0,     0,
     232,     0,     0,   243,  1761,  1761,     0,   245,     0,   213,
     127,   130,     0,   160,   155,   118,   117,  1783,   660,   660,
       0,     0,  1766,     0,     0,     0,  1184,   349,     0,   488,
     490,   491,   590,     0,   360,   359,   353,   361,    24,  1718,
    1734,   333,   325,  1734,  1718,  1748,  1734,  1702,     0,   295,
     297,   300,   301,   302,   303,   304,   305,   306,   307,   308,
       0,  1655,     0,     0,  1655,  1659,     0,  1655,  1655,  1655,
       0,  1650,  1655,  1655,  1655,     0,  1655,  1655,  1655,  1655,
    1624,   210,   211,   209,   208,     0,   196,     0,   197,     0,
     201,   222,   221,   222,   218,  1560,  1559,   236,   237,  1619,
    1672,   239,     0,   242,  1762,   246,   247,   248,  1557,     0,
     129,  1784,  1734,  1713,   660,   657,   666,     0,   660,   674,
     649,    16,    14,  1610,   587,   494,   492,   592,   354,   362,
     363,   355,     0,    25,   358,     0,     0,     0,  1702,  1702,
       0,   326,     0,     0,     0,  1734,     0,  1703,     0,     0,
     298,   299,  1644,  1517,  1656,     0,  1653,  1668,  1651,  1642,
    1652,  1666,  1643,  1645,  1646,  1663,  1664,  1647,  1648,  1649,
    1641,  1672,   206,   205,   195,   207,     0,   220,   219,  1695,
       0,   238,  1673,  1734,     0,  1558,   214,   145,   146,   141,
     147,   144,   142,   143,   131,   132,     0,   658,  1734,     0,
     671,   663,   661,   662,   675,  1611,  1182,  1734,   361,   596,
     591,   593,   594,   371,   365,    26,   320,   345,     0,  1691,
    1690,  1692,  1693,   324,   323,   322,  1746,  1716,  1730,     0,
     346,   347,   329,   330,  1734,  1734,  1657,   212,     0,   225,
    1694,     0,   240,  1554,     0,   116,     0,  1734,   672,     0,
       0,   650,  1734,  1734,  1702,   505,   530,   495,   498,   499,
       0,   597,   595,   356,  1706,     0,     0,   310,   334,  1747,
     336,     0,   313,  1731,  1781,   344,     0,     0,     0,   241,
    1555,   133,   667,   668,   670,     0,   659,   676,   678,     0,
     503,   503,   506,   507,  1736,   493,   496,   599,     0,   466,
     399,   434,   422,   431,   428,   425,   400,  1795,   401,   402,
     403,   404,   405,   406,   407,   408,   409,   436,   410,  1772,
       0,   435,     0,   411,   394,   412,   413,     0,     0,   415,
     416,   414,   462,   418,   419,   417,  1734,  1736,    24,   372,
     387,   373,   374,   375,   376,   377,   395,   378,   379,   380,
     381,   382,   383,   384,   385,   386,     0,     0,  1707,     0,
     366,   321,   311,   312,   309,   335,  1716,  1782,   318,   331,
     327,   332,   328,     0,   226,   227,   223,   669,   678,     0,
     497,  1599,  1600,  1601,   504,   500,   516,   501,     0,   361,
     531,  1778,   481,   432,   433,   420,   421,   429,   430,   426,
     427,   423,   424,  1796,     0,  1773,   460,   398,   447,   389,
     479,   463,   464,   461,     0,     0,   357,   391,   393,   388,
    1686,  1686,     0,  1752,  1752,   370,   367,  1627,   337,   338,
     339,   340,  1708,     0,   314,  1729,   320,     0,   224,     0,
       0,   690,   686,   679,   680,     0,   681,   683,     0,     0,
       0,   682,   502,   519,   526,   363,   533,   613,  1734,   603,
     601,   602,   605,  1750,  1734,     0,  1734,     0,   617,   608,
    1750,   619,   609,     0,     0,   612,  1714,   616,   610,   615,
    1734,   614,     0,   611,   618,     0,  1736,   598,   630,   625,
     628,   627,   626,   629,   467,   472,   600,   631,   476,   474,
     465,     0,  1791,     0,   397,   396,   470,     0,   390,   437,
     438,  1628,     0,     0,  1709,     0,  1746,  1716,   319,     0,
     685,   692,   689,   693,   759,   820,   829,   835,   841,   888,
     843,   892,   906,   924,   917,   925,   926,   932,   967,   976,
     989,  1017,  1028,  1048,  1050,  1793,  1056,  1060,  1073,  1094,
    1096,  1132,  1134,  1138,     0,  1144,  1159,  1187,     0,  1214,
       0,  1216,  1220,  1227,  1228,  1244,  1264,  1282,  1294,  1303,
    1314,  1322,  1324,  1326,  1328,  1333,  1355,  1378,  1441,   684,
     702,   703,   704,   705,   706,   707,   709,   708,   710,   711,
     713,   712,   714,   715,   716,   717,   718,   719,   720,   721,
     722,   723,   724,   725,   726,   727,   728,   729,   730,   731,
     732,   733,   734,   735,   736,   737,   739,   738,   740,   741,
     742,   743,   744,   745,   746,   747,   748,   749,   750,   751,
     752,   753,   754,   755,  1351,  1352,  1353,   756,   757,   701,
     517,   518,   510,   511,   508,   509,   528,   529,  1778,     0,
    1751,  1734,     0,   606,   607,     0,   633,  1734,   604,  1791,
    1734,     0,   634,   632,     0,   473,     0,   448,  1792,   440,
     480,  1795,   392,   369,   368,     0,     0,  1708,   316,     0,
     694,     0,     0,     0,  1460,   837,   849,   890,   849,  1597,
     908,     0,   928,     0,  1797,     0,     0,  1460,  1019,  1035,
       0,     0,  1052,  1058,     0,     0,   516,     0,     0,     0,
       0,   758,  1146,  1169,     0,  1212,     0,  1213,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1323,  1321,   516,
       0,     0,     0,  1356,  1362,     0,     0,     0,  1734,     0,
     520,     0,     0,   565,  1750,   562,   585,   580,  1702,     0,
       0,  1734,  1752,  1734,     0,   532,   538,   539,   540,   549,
     541,   543,   546,   534,   535,   536,   542,   545,   566,   547,
     550,   537,     0,   544,   548,  1606,   624,  1604,  1605,   642,
     635,   623,   637,   647,  1610,   636,   468,   475,   454,  1754,
     455,  1712,   439,   441,   442,   444,   445,   452,   450,  1739,
     446,  1774,  1746,  1746,     0,  1754,   315,   677,   687,   818,
     786,  1752,     0,  1752,   827,  1579,   825,  1569,  1571,  1577,
    1578,  1674,   830,     0,  1459,     0,   836,     0,   889,     0,
     893,   894,  1598,   907,   920,     0,  1536,  1684,     0,  1767,
    1709,     0,   942,   965,  1396,   939,   941,     0,     0,   974,
       0,   849,   995,   994,     0,   991,   993,  1025,  1022,  1020,
    1024,  1018,  1037,  1036,     0,  1033,  1752,  1049,  1564,  1567,
    1051,  1547,  1771,  1770,  1057,     0,  1064,  1061,  1789,  1458,
    1075,  1095,     0,  1098,  1099,  1100,  1133,  1268,     0,  1135,
       0,  1142,     0,  1145,  1170,  1553,  1617,  1160,  1169,  1162,
       0,  1167,  1549,  1551,     0,  1550,     0,  1676,  1380,  1539,
    1767,  1380,     0,  1242,  1233,  1540,     0,  1245,  1246,  1247,
    1248,  1249,  1257,  1250,  1260,     0,  1544,     0,  1265,  1191,
    1299,     0,  1295,  1712,     0,  1305,  1307,     0,  1319,     0,
    1325,     0,  1330,  1335,  1363,     0,  1364,  1754,  1380,     0,
       0,  1396,     0,   514,     0,   512,   513,  1734,  1734,   527,
     563,   564,   561,  1734,   586,   584,   581,   582,  1736,   574,
    1734,  1460,     0,     0,     0,     0,   567,     0,     0,   572,
     575,   578,   645,   643,   644,   646,  1602,   640,   638,   639,
     641,  1602,   620,  1795,     0,     0,   443,   453,  1734,  1775,
    1793,   343,   342,  1746,     0,   819,   760,     0,  1362,   790,
    1396,   787,   788,     0,     0,     0,     0,   828,   821,     0,
       0,  1570,  1675,   833,  1674,  1493,  1494,  1495,  1489,  1472,
    1475,  1473,  1474,  1793,  1503,  1499,  1479,  1787,  1500,  1480,
    1787,  1481,  1497,  1488,  1492,  1491,  1490,  1496,  1498,  1467,
    1468,  1469,  1470,  1471,  1501,  1465,  1466,  1504,  1506,  1461,
    1462,  1476,  1477,  1478,  1464,  1553,  1617,     0,   851,   891,
     851,   897,   910,   921,   918,   923,   922,  1460,  1537,  1685,
    1538,   930,  1432,   782,  1591,  1592,  1593,   946,   945,   944,
     943,   966,   933,  1397,   940,   938,  1399,   941,  1396,  1396,
    1396,  1396,  1750,  1750,  1750,   780,   781,  1734,   949,   963,
     964,   951,   948,     0,  1734,   954,   955,     0,   947,   958,
     956,     0,   957,   975,   968,     0,     0,   978,  1460,  1460,
    1015,   996,   997,   999,  1023,  1026,  1040,  1034,  1031,     0,
    1565,     0,  1054,  1059,  1790,  1062,  1077,     0,  1700,  1121,
    1106,  1097,  1101,  1103,  1104,  1105,  1272,     0,     0,  1143,
    1139,     0,  1151,  1148,  1150,  1149,  1152,     0,  1161,  1166,
     695,  1164,  1460,     0,     0,     0,  1171,     0,  1677,  1678,
    1767,     0,  1215,  1189,  1222,  1235,  1243,  1229,     0,  1235,
       0,  1258,  1261,     0,     0,  1545,  1256,     0,  1255,     0,
       0,  1283,     0,     0,     0,  1296,  1300,  1299,  1297,     0,
       0,  1306,     0,  1320,  1315,     0,     0,  1331,  1332,  1329,
    1712,     0,     0,  1365,     0,     0,  1222,     0,  1797,  1450,
    1399,   522,   515,     0,     0,     0,   573,     0,   570,  1684,
     568,   569,     0,   555,     0,     0,   551,     0,   579,   577,
     576,  1626,  1609,   622,  1603,  1607,  1608,   621,  1774,   449,
     459,   456,   457,     0,   477,   341,   317,   776,  1396,   767,
     770,   764,   773,   765,   768,   771,     0,  1396,     0,   763,
     772,   779,   778,     0,   761,  1399,   789,   809,  1734,   793,
     795,   791,   792,     0,  1734,   811,   798,   804,   799,     0,
     802,   815,   812,   800,     0,   813,   810,   801,   814,   803,
    1588,  1587,     0,  1576,     0,  1572,  1573,  1574,  1405,  1684,
       0,     0,   831,     0,  1502,  1788,  1505,  1507,  1485,  1486,
    1487,  1482,  1483,  1484,  1463,     0,   839,   855,  1741,   858,
     845,   860,   898,   900,   895,   915,   916,   909,   911,     0,
    1405,   931,   929,  1437,  1439,   930,  1433,  1434,  1435,  1402,
    1400,  1394,   936,   937,   934,   935,     0,     0,     0,     0,
     952,   953,     0,   950,  1456,  1456,     0,  1405,  1684,   980,
     987,   979,   992,  1010,  1005,  1012,  1011,  1003,  1006,  1007,
    1013,  1016,   990,   998,  1460,   695,  1021,     0,     0,  1029,
    1038,  1032,     0,  1568,  1566,  1754,  1053,  1548,   695,  1793,
    1085,  1086,  1088,  1090,  1091,  1087,  1089,  1080,  1793,  1076,
    1701,     0,  1122,     0,  1124,  1123,  1125,  1116,  1117,     0,
       0,  1102,  1274,  1758,     0,     0,  1136,  1405,  1684,  1797,
       0,  1618,  1165,   696,  1177,  1172,  1176,  1173,  1178,     0,
    1168,  1552,  1387,  1386,  1175,  1198,  1381,     0,     0,  1432,
       0,     0,     0,  1234,     0,     0,  1794,  1259,     0,  1263,
    1262,  1253,     0,  1192,  1285,     0,     0,  1193,     0,  1196,
    1302,  1301,  1298,  1308,  1309,  1310,     0,  1405,  1684,     0,
    1670,     0,  1310,  1377,  1367,  1366,  1372,     0,  1374,  1375,
    1382,     0,  1454,  1451,  1442,  1395,     0,   525,   523,   560,
     559,   583,   556,   554,   557,   553,   558,   552,     0,  1793,
     458,   451,  1734,   471,   777,   766,   769,  1396,   774,   762,
     817,  1393,     0,   796,   797,     0,   794,  1734,  1456,  1456,
    1590,  1575,  1589,  1684,  1406,   822,  1408,  1405,   834,     0,
       0,     0,   856,   857,  1741,  1742,   854,   859,   855,   878,
     862,   878,   861,     0,     0,   899,   901,   904,     0,   914,
       0,   919,   695,   695,   927,  1436,   695,  1403,   695,  1720,
    1720,  1720,   960,   959,   962,     0,   961,     0,   969,     0,
     868,     0,   977,  1460,  1460,  1009,  1004,  1002,  1000,  1027,
    1044,  1043,  1042,  1041,  1046,  1039,  1045,     0,   695,  1065,
    1071,  1063,     0,     0,  1092,     0,     0,     0,  1118,  1120,
       0,  1112,  1128,  1113,  1114,  1107,  1108,  1128,  1266,  1734,
       0,  1759,  1273,  1739,  1137,  1140,     0,     0,  1157,  1163,
     700,     0,     0,     0,     0,  1189,  1190,  1421,  1423,  1218,
    1418,  1419,  1225,  1223,     0,  1460,  1236,  1460,  1230,  1238,
    1251,  1252,  1254,  1734,  1432,  1195,  1197,  1194,  1412,     0,
    1684,  1316,     0,     0,  1671,  1756,  1337,     0,  1340,  1343,
    1347,  1341,  1373,  1754,  1376,  1388,  1698,  1448,     0,  1452,
    1734,  1734,     0,   477,     0,   775,   806,   805,     0,   808,
     807,  1405,   695,  1409,  1404,   823,   832,   840,   838,   853,
    1741,  1368,   877,  1369,   876,   846,   847,   848,     0,   881,
     879,   868,   881,   902,   903,     0,   896,   912,   913,  1438,
    1440,  1398,  1401,  1721,   784,   783,   785,  1743,  1405,  1536,
    1405,  1536,   666,   981,   982,     0,   988,  1008,  1014,  1047,
    1030,  1055,  1070,  1071,   695,  1072,  1066,  1078,  1081,  1082,
    1722,  1093,  1074,  1079,     0,  1128,     0,  1111,  1109,  1110,
    1115,  1276,     0,  1760,  1270,  1405,  1153,  1158,  1154,   697,
    1179,     0,  1199,     0,     0,  1200,     0,  1203,  1191,   695,
     695,  1219,  1217,  1420,  1226,  1221,  1224,  1231,   695,  1240,
    1239,  1679,  1292,  1413,  1312,  1415,     0,  1405,  1405,  1327,
    1594,  1595,  1596,  1757,  1670,  1339,  1730,  1345,  1730,  1412,
       0,  1428,  1430,  1391,  1389,  1425,  1426,  1390,  1699,     0,
    1730,  1444,  1455,     0,     0,     0,     0,   571,   469,   478,
     816,   824,  1407,   695,   852,  1752,     0,   873,  1370,  1371,
     884,   882,   695,   863,   864,   666,   886,   905,  1744,  1745,
    1457,   971,     0,   970,     0,   984,     0,   983,   870,   871,
     869,   872,  1069,  1067,  1071,  1083,  1723,     0,  1128,  1119,
    1128,  1130,     0,     0,  1279,  1275,  1269,  1141,  1147,     0,
       0,  1202,  1204,  1201,  1205,  1422,  1424,   695,  1237,   695,
    1680,     0,     0,  1293,  1284,   695,  1313,  1304,  1416,  1411,
    1311,  1318,  1317,  1338,     0,  1730,  1342,     0,  1349,  1361,
    1358,  1360,  1359,  1354,  1357,   695,   695,  1392,  1379,  1427,
    1385,  1384,  1743,     0,  1453,  1446,  1445,  1734,   521,   524,
    1410,     0,   874,   885,   842,   695,   880,   865,   866,     0,
     887,   844,  1405,  1405,   666,   666,  1068,     0,  1127,  1126,
    1131,     0,  1734,  1277,     0,     0,  1267,  1271,  1156,     0,
       0,  1734,  1207,   699,  1232,  1241,  1286,  1508,  1787,  1510,
    1787,  1287,  1288,  1289,  1290,  1291,  1414,   695,  1344,     0,
    1348,  1350,  1334,  1429,  1431,  1383,  1449,  1447,     0,   875,
     883,   666,   973,   972,   986,   664,  1084,  1129,     0,  1280,
    1734,  1155,  1460,     0,  1210,  1208,  1209,  1505,  1507,  1417,
    1346,  1443,   664,   665,   985,  1278,     0,  1180,  1206,  1211,
    1188,     0,     0,   867,  1281,  1793,  1793,  1509,  1511
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     2,     6,     7,    29,   157,   293,   417,   517,
     516,     8,    15,   176,    14,    10,   714,   602,    90,   242,
     310,   311,    13,    30,    31,    32,   184,    33,    34,    35,
     124,   125,   105,   106,    36,   107,   108,   109,   110,   471,
     111,   112,    37,   115,   116,   117,   200,    38,   141,   142,
     143,   144,   145,   146,   408,   512,   597,   147,   269,   504,
     505,   506,   689,   774,   507,   775,   148,   280,   281,   282,
     510,   149,   396,   397,   150,   151,   152,   153,   154,   155,
     156,    39,    40,    41,    42,    43,   205,   266,   376,   377,
     669,   572,   754,   378,   664,   379,   380,   381,   382,   383,
     674,   757,   956,   384,   385,   386,   387,   388,   389,   680,
     390,   391,   392,   587,    44,    45,    46,    47,    48,    49,
      74,    50,   252,   253,   357,    51,    61,    80,   165,   166,
      81,   167,   168,   169,   241,   302,   170,   303,   425,   529,
     630,   631,   944,   632,  1386,  1016,   847,   633,   805,   720,
     634,   815,   950,   635,   717,   636,   945,  1008,  1009,  1010,
    1011,   637,   638,   639,   171,   420,   423,   424,   527,   616,
     793,   918,   617,   711,   712,   844,  1005,   618,   843,   919,
     920,   921,   922,  1088,   923,   924,   925,   926,   927,   928,
    1372,  1373,  1374,  1082,  1375,  1376,  1377,  1378,  1379,  1380,
    1841,  1842,   929,   930,   993,   931,   932,  1073,  1573,   933,
    1251,  1074,  1075,  2103,   934,   935,   172,   419,   173,   418,
     239,   525,   609,   610,   611,   787,   837,   838,   965,   966,
     874,  1226,  1227,  1537,  1538,  1032,   839,  1221,  1320,  1033,
     875,   970,  1228,  1343,  1344,  1826,  1345,  1346,  1347,  1348,
    1820,  1349,  1350,  1351,  1352,  1559,  1560,  1548,  1353,  1354,
    1545,   705,   301,   612,   707,   790,   791,   792,   877,   971,
    1076,  1571,  1566,  1077,   232,   784,   868,   515,   599,   598,
     694,   695,   696,  2604,   780,   829,   601,   831,   958,   959,
    1023,  1024,  1025,  1026,  1027,  1028,  1261,  2553,  2023,  2220,
    2554,  1029,  1030,  1160,  1161,  1262,  1389,  1422,  1685,  1686,
    1590,  1591,  1592,  1586,  1162,  1263,  1394,  1600,  1598,  1163,
    1264,  1402,  1902,  1164,  1265,  1406,  2131,  1165,  1266,  1166,
    1268,  2139,  2295,   412,  1920,  2134,  2138,  2141,  2142,  2301,
    2433,  2434,  2322,  2297,  2298,  2299,  2432,  2430,  2525,  2524,
    2531,  1167,  1267,  1408,  1168,  1269,  1410,  1651,  2147,  1924,
    2145,  2306,  1169,  1270,  1413,  1927,  1928,  1170,  1271,  1414,
    1654,  1657,  1171,  1172,  1173,  1273,  1418,  1932,  1174,  1274,
    1423,  1424,  1425,  1672,  1175,  1275,  1429,  1704,  1176,  1276,
    1960,  1961,  2170,  2323,  2324,  2172,  1177,  1277,  1434,  1435,
    1710,  1711,  1712,  1975,  2178,  1713,  1967,  1968,  1969,  2175,
    1972,  1178,  1278,  1441,  1715,  1976,  1179,  1279,  2184,  1716,
    1444,  1445,  1979,  1980,  2330,  1180,  1280,  1181,  1281,  1182,
    1451,  1986,  1183,  1283,  1454,  1184,  1284,  1457,  1988,  2191,
    2336,  1185,  1285,  1726,  1997,  2194,  2338,  2339,  2340,  2342,
    1186,  1286,  1187,  1287,  1462,  1731,  1732,  1733,  2010,  2205,
    2206,  1734,  2007,  2008,  2009,  2199,  1735,  2347,  2541,  1188,
    1288,  1189,  1289,  1469,  1190,  1290,  1471,  1740,  1191,  1292,
    1473,  1746,  2020,  2468,  2358,  1192,  1293,  1477,  1750,  1751,
    1478,  1479,  1480,  2025,  2027,  2028,  1193,   605,   520,   521,
    1194,  1294,  2038,  1781,  2225,  2552,  2594,  2610,  1195,  1196,
    1197,  1296,  1198,  1298,  2372,  1199,  1299,  2039,  2375,  1200,
    1201,  1300,  1493,  2477,  1769,  2041,  2378,  2238,  2239,  2479,
    1767,  1202,  1301,  1497,  1498,  1499,  1500,  1779,  1501,  1502,
    1503,  1504,  1203,  1302,  1466,  2351,  1736,  2466,  2012,  2208,
    2464,  2546,  1204,  1303,  2054,  2244,  2481,  2484,  1205,  1304,
    1512,  1785,  1206,  1305,  1514,  1515,  2248,  2487,  1207,  1306,
    1518,  1794,  1208,  1308,  1209,  1309,  1210,  1310,  1211,  1311,
    1799,  1212,  1312,  1801,  2255,  2256,  2072,  2258,  2397,  2496,
    2399,  2572,  1213,  1214,  1314,  2503,  1526,  1804,  2076,  2300,
    2431,  2263,  1215,  2078,  1216,  1217,  1316,  1762,  2265,  2461,
    2403,  2508,  1864,  1675,  1809,  1676,  1939,  1941,  2158,  2156,
    2125,  2126,  2282,  2284,  2423,  2384,  2385,  2485,  2489,  2567,
    2229,  2230,  2369,  2231,  2370,  2404,  2405,  2505,  2406,  2506,
    1935,  1936,  1937,  2152,  1938,  2153,  1218,  1317,  1531,  2517,
    2411,  2084,  2414,  2269,  2164,  1458,  1459,  1404,  1405,  1639,
    1640,  1641,  1642,  1643,  2564,  2565,   344,   345,   530,   642,
     531,  1395,  1415,  1658,  1488,  1494,   182,   183,  1505,   413,
    1722,  1481,  1482,  1483,   822,   676,   766,   677,   283,   287,
    1447,  1448,  1396,  1417,   348,  1892,  2121,  2207,  2389,  1411,
     960,  1833,  1356,  1834,   786,  1398,   179,  1485,   180,   249,
     250,  1358,  1836,  1399,  1007,   351,  1400,   431,   434,   535,
     550,   545,   541,   528,  2257,   761,  1603,  1760,  2482,    77,
    1660,   289,   246,   721,   762,   197,  2409,  2001,   728,   119,
     939,  1427,   394,   697,   410,   224,   186,  2314,  2457,    68,
     204,  1017,   854,   264,   189,   406,   485,  2136,  2440,   850,
     473,  1231,   436,    95,  2394,  2212,   685,   113,   603,   475,
    1455,   986,  1580,   214,   937,   948,   692,   100,  1906,  1725,
    1249,  1721,   984,   127
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -2162
static const yytype_int16 yypact[] =
{
   -2162,   220,   -21, -2162,  -254,  -254,   -21, -2162, -2162, -2162,
   -2162, -2162, -2162,  1122,   773,   773,   393,  -134,   778,   261,
     430,   605,   586,   647,   671,   955,  1205,   747,   760,  1003,
   -2162,   573, -2162,  1121, -2162, -2162, -2162, -2162, -2162,   516,
   -2162,   450, -2162, -2162,   526, -2162, -2162,    38, -2162, -2162,
   -2162, -2162, -2162, -2162, -2162,   926,   926, -2162, -2162,   847,
   -2162,   989,   856,   859, -2162,   908, -2162, -2162,   980,   354,
    1208, -2162, -2162, -2162,  1404,  1133, -2162,   979,    -2,  1015,
   -2162,   752, -2162, -2162, -2162, -2162, -2162, -2162, -2162,  1226,
    -254, -2162, -2162,  1042, -2162,  1054, -2162, -2162, -2162, -2162,
    1442,  1161, -2162,  1332, -2162,   757,  1096, -2162,   844, -2162,
   -2162, -2162, -2162,   134,    64, -2162,   -60, -2162, -2162,  1380,
   -2162, -2162, -2162, -2162, -2162,  1085,   951,  1475,   979,   979,
    1184,  1232,  1332,  1332,  1332,   979,  1237,  1332,  1363,  1550,
    1193, -2162,   609, -2162, -2162,  -135, -2162, -2162, -2162, -2162,
   -2162, -2162, -2162, -2162, -2162, -2162, -2162,  1318,  1523, -2162,
    1280,  1294, -2162,  1298,  1304,  1302, -2162, -2162,   509, -2162,
   -2162, -2162, -2162, -2162, -2162, -2162,   746,    17, -2162, -2162,
     -48, -2162,  1054, -2162, -2162, -2162,  1054,  1332, -2162,  1414,
   -2162,   984, -2162, -2162, -2162,  1293, -2162,  1332,  1455, -2162,
     399, -2162, -2162, -2162,  1054, -2162, -2162, -2162,  1199,  1419,
   -2162,  1332,  1332, -2162,  1332,  1042,  1614,  1042,  1332,  1459,
   -2162,  1042, -2162, -2162,   979,  1332,    -8, -2162, -2162,  1313,
    1313,  1568,   -21,  -254,  1213,  1214,  1215,  -254,  1326,  1334,
   -2162,  1054,  -254,   130, -2162, -2162,  1042, -2162,  3844,  1222,
   -2162, -2162,  1054, -2162,  1391,  1414, -2162, -2162, -2162, -2162,
    1042, -2162, -2162, -2162, -2162,  1054,  1187, -2162, -2162,  1332,
     706,  1042,  1460, -2162,  1582, -2162, -2162,  1042, -2162, -2162,
   -2162,   979,    88, -2162,   979, -2162, -2162, -2162,  1652, -2162,
   -2162,  1452, -2162,  1011, -2162, -2162, -2162, -2162, -2162,  1233,
    1342, -2162, -2162, -2162, -2162, -2162,  -146, -2162, -2162, -2162,
    1393, -2162,  1582, -2162,  6932,  1239,  1244, -2162,  1248,  1254,
   -2162,  1428,    17, -2162,  1256,  1258, -2162,  1259,  1262, -2162,
    1263, -2162,  1268,  1270, -2162,  1271,  1272,  1273,  1275,  1244,
   -2162,  6932,  6932,  6932,    15, -2162,   100, -2162, -2162, -2162,
    1261, -2162, -2162,  6932, -2162, -2162,  1414, -2162,    20, -2162,
    1485,  1418,   847, -2162, -2162,    76,  1485,  1332,  1652,    63,
     466,  1028,  1295, -2162, -2162, -2162, -2162, -2162, -2162, -2162,
   -2162, -2162,  1359, -2162, -2162, -2162, -2162, -2162, -2162, -2162,
   -2162, -2162, -2162,  1332,  1378,  1470,   706, -2162,  1341, -2162,
   -2162, -2162, -2162, -2162, -2162, -2162,  1414, -2162, -2162, -2162,
   -2162, -2162,    24, -2162, -2162, -2162, -2162,  1447, -2162, -2162,
   -2162, -2162,  1284, -2162, -2162, -2162, -2162, -2162,   552,  6932,
    6932, -2162,  6932,  6932, -2162, -2162,  7713,  1301,  6932,  6932,
    6932,  6932,  6932,  6932,  6932,  6932,  6932,  6932,  6932, -2162,
      51,    51,   514, -2162, -2162, -2162,  3844,  6932,  6932,  6932,
    6932,  6932,  6932,  6932,  6932,  6685,   706,   100, -2162, -2162,
   -2162, -2162, -2162,  1332, -2162,  1534,  1283, -2162, -2162,  1332,
     689,  1332,  1332, -2162,  1388,  1332,  1332, -2162,  1404,   415,
    1332,  1305, -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162,
   -2162, -2162, -2162, -2162, -2162,   716, -2162,    49, -2162,   706,
    1414, -2162,  1282, -2162, -2162,  1413,   773,   773, -2162, -2162,
    1447, -2162, -2162, -2162,  -254,  1448, -2162,   -91,   505,  1349,
      77,   552,   113,    84,    95,  1289, -2162,  1261, -2162, -2162,
     373,  1296,   619,    98,   532,  1297,   803,   117,   162,   779,
    1306,   854,   897,  1029, -2162, -2162,   552,   552,   552,   552,
     149,   149,    51,    51,    51, -2162,  1048, -2162,   572,  1332,
   -2162, -2162,   968,   782,  1397,  1747, -2162, -2162,   520,  1387,
   -2162,  1320,  1042, -2162,  1488,  1488,  1495, -2162,  1042, -2162,
   -2162, -2162,   716, -2162, -2162, -2162, -2162,  1389,   161,   161,
    1042,  1311, -2162,  1312,  1317,  1545, -2162, -2162,  1331, -2162,
    1448, -2162, -2162,   505, -2162, -2162, -2162,   -66,  1319,  1442,
    1332, -2162,  1243,  1332,     6,  1485,  1332,  1784,  1514, -2162,
   -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162,
    1310,  1244,  6932,  6798,  1244, -2162,  1042,  1244,  1244,  1244,
    6932, -2162,  1244,  1244,  1244,  -107,  1244,  1244,  1244,  1244,
   -2162, -2162, -2162, -2162, -2162,  1320,   131,  1804,   178,   251,
   -2162,  1343, -2162,  1343, -2162, -2162, -2162, -2162, -2162,   -87,
     104, -2162,  -205, -2162, -2162, -2162, -2162, -2162,  1042,   987,
   -2162, -2162,  1332, -2162,   151, -2162,  -170,   -13,   328,  1582,
   -2162, -2162, -2162,  1558, -2162, -2162, -2162,   505, -2162, -2162,
    1723, -2162,  1729, -2162,  1337,  1414,  1353,  1243,   714,   714,
    -108, -2162,   520,  1414,  1360,  1332,  1331, -2162,  1331,   143,
   -2162, -2162, -2162,   552, -2162,  1058, -2162, -2162, -2162, -2162,
   -2162,   552, -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162,
   -2162,   104, -2162, -2162, -2162,  1582,   -80, -2162, -2162, -2162,
    1692, -2162, -2162,  1332,  1042, -2162, -2162, -2162, -2162, -2162,
   -2162, -2162, -2162, -2162,  1811, -2162,   979, -2162,  1332,  1434,
    1529, -2162, -2162, -2162,  1716, -2162, -2162,   167,   -66, -2162,
   -2162,   505, -2162, -2162,    10, -2162,  1403, -2162,  1042, -2162,
   -2162, -2162, -2162, -2162, -2162, -2162,  1584,    53,  1616,  1366,
   -2162, -2162, -2162, -2162,  1332,  1332, -2162, -2162,  1541, -2162,
   -2162,  1042,  1042, -2162,   987, -2162,   794,  1332, -2162,  1370,
    1358, -2162,  1332,  1332,  1784,  1241, -2162, -2162, -2162, -2162,
    1636, -2162, -2162,  7401,  1817,  1641,  1414,   200,  1042, -2162,
   -2162,  1414, -2162, -2162,  1457, -2162,   522,   522,   663,  1042,
   -2162, -2162, -2162, -2162, -2162,  1414, -2162, -2162, -2162,  7713,
    1658,  1658, -2162, -2162,   741,   505, -2162, -2162,  1617, -2162,
   -2162,   700,   734,   744,   883,   964, -2162,  1394, -2162, -2162,
   -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162,  1491,
    1042, -2162,  1414, -2162, -2162, -2162, -2162,  1042,  1042, -2162,
   -2162, -2162,   885, -2162, -2162, -2162,  1332,   741,  1319, -2162,
   -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162,
   -2162, -2162, -2162, -2162, -2162, -2162,   -38,   -96, -2162,    42,
   -2162, -2162, -2162, -2162, -2162,   288,  1550, -2162,    14, -2162,
   -2162,  1582, -2162,  1269, -2162, -2162, -2162, -2162, -2162,  2859,
   -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162,  1414,   254,
   -2162,  4407, -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162,
   -2162, -2162, -2162, -2162,  1382, -2162, -2162, -2162,  1433, -2162,
     -30, -2162, -2162, -2162,  2308,   175,  1337,  1838, -2162, -2162,
    1313,  1313,   706,  1428,  1428, -2162, -2162,  1261, -2162, -2162,
   -2162, -2162,  1836,  1673, -2162, -2162,  1403,  1414, -2162,  3295,
    1390,   912, -2162, -2162, -2162,   970, -2162, -2162,  1510,  3960,
    4750, -2162,  1042, -2162,  1179, -2162, -2162, -2162,  1332, -2162,
   -2162, -2162, -2162,  1601,  1332,  1446,  1332,  7629, -2162, -2162,
    1601, -2162, -2162,  1855,  1414, -2162,  1652, -2162, -2162, -2162,
    1332, -2162,  1042, -2162, -2162,  1042,   741, -2162, -2162, -2162,
   -2162, -2162, -2162, -2162,   175, -2162, -2162, -2162, -2162,  1465,
   -2162,  1414,  1464,  1042, -2162, -2162,   175,  1640, -2162, -2162,
   -2162, -2162,  1042,  1042, -2162,    28,  1584,  1550, -2162,  1793,
   -2162, -2162, -2162,  1655, -2162, -2162, -2162, -2162, -2162, -2162,
   -2162, -2162, -2162, -2162, -2162, -2162,  1480, -2162, -2162, -2162,
   -2162, -2162, -2162, -2162, -2162,   847, -2162, -2162, -2162, -2162,
   -2162, -2162, -2162, -2162,  1527, -2162, -2162, -2162,  1476, -2162,
    1479, -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162,
   -2162,  1589, -2162, -2162, -2162, -2162,  1144, -2162, -2162, -2162,
   -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162,
   -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162,
   -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162,
   -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162,
   -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162,
   -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162,
   -2162,     4, -2162, -2162, -2162, -2162,  1414, -2162,  7264,   551,
   -2162,  1332,  1042, -2162, -2162,   551, -2162,  1332, -2162,  1464,
    1332,   551, -2162, -2162,   175, -2162,    67, -2162, -2162,  1183,
    1582,  1394, -2162, -2162, -2162,   520,   520,  1836,  1798,  1437,
   -2162,  1439,  1042,  5631,  1449, -2162,  1452, -2162,  1452,  7713,
   -2162,  6360, -2162,  1054,  4510,  6360,  1665,   318,  1242,    90,
      61,  1042, -2162,  5095,  2510,    61, -2162,  1377,  1042,  5719,
    6360, -2162, -2162,  7335,  1054, -2162,  1042, -2162,  1054,  1042,
      71,    44,  1042,  1054,   871,  5807,  5893, -2162, -2162, -2162,
    1042,  1054,  7713, -2162,   212,  1813,  1042,   558,  1332,  1364,
   -2162,  1562,  1080, -2162,  1316, -2162,  1706,  1373,  1784,  1719,
    1462,  1332,  1428,  1332,  1042, -2162, -2162, -2162, -2162, -2162,
   -2162, -2162, -2162, -2162, -2162, -2162, -2162,  1516, -2162, -2162,
   -2162, -2162,   -45, -2162, -2162, -2162, -2162, -2162, -2162,   890,
   -2162, -2162,   915, -2162,  1558, -2162,   175, -2162, -2162,   989,
   -2162,  1880, -2162,  1183, -2162, -2162, -2162, -2162,   145,  1534,
   -2162,  1547,  1584,  1584,   520,   989, -2162, -2162, -2162,  1794,
    3081,  1428,  1042,  1428,  1796, -2162,  6082, -2162, -2162, -2162,
   -2162,  1713, -2162,  1874, -2162,  3640,   535,  7713,  7713,  7713,
   -2162, -2162, -2162,  1054,  1797,  2770, -2162,  1570,  1054,  1418,
    7713,   164, -2162,  1795,  4955, -2162,  1143,   644,  1645,  1799,
     624,  1452, -2162, -2162,    48, -2162, -2162, -2162,  1837, -2162,
   -2162, -2162, -2162, -2162,  5254, -2162,  1428,    68, -2162, -2162,
   -2162, -2162, -2162, -2162, -2162,  6360, -2162, -2162,  1520, -2162,
      41,  1042,   142, -2162, -2162, -2162, -2162,  1582,  6360, -2162,
    1518,  1801,  1893,  1221, -2162,   297,   473, -2162,  7615,  3004,
     274,  1524, -2162,    17,  1526, -2162,  1533,   -61,  1754,  1582,
    1418,  1754,  1042,  1802,  1492,  1582,  7713, -2162, -2162, -2162,
   -2162,  1452, -2162,    61, -2162,   839, -2162,    52, -2162,   992,
    5342,   140, -2162,  1880,  5356, -2162, -2162,  1042,  1803,  6096,
    1042,  1759,  1300,  1846, -2162,  1605,  1564,   989,  1754,  1042,
    7713,  1787,  1414, -2162,  1757, -2162, -2162,  1332,  1332, -2162,
   -2162, -2162, -2162,  1332, -2162, -2162, -2162, -2162,   741, -2162,
    1332, -2162,  1042,  1042,   777,  1771, -2162,  1659,  1414,   -45,
   -2162, -2162, -2162, -2162, -2162, -2162,   568, -2162, -2162, -2162,
   -2162,   568, -2162,  1394,  1042,  1500, -2162, -2162,  1332, -2162,
     847, -2162, -2162,  1584,  1042, -2162, -2162,  1440,  1966, -2162,
    1787,  7489, -2162,  3296,   -52,  1544,  7713, -2162, -2162,  6360,
    1770, -2162, -2162,  1618,  1713, -2162, -2162, -2162, -2162, -2162,
   -2162, -2162, -2162,   847, -2162, -2162, -2162,  1552, -2162, -2162,
    1552, -2162, -2162,  1472, -2162, -2162, -2162, -2162, -2162, -2162,
   -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162,  3640,
   -2162, -2162, -2162, -2162, -2162, -2162,    17,  1549,   991, -2162,
     991,   127,   475, -2162, -2162, -2162, -2162, -2162, -2162, -2162,
   -2162,  1835,  1071, -2162, -2162,  1261, -2162, -2162, -2162, -2162,
   -2162, -2162, -2162, -2162, -2162, -2162,  1699, -2162,  1787,  1787,
    1787,  1787,  1601,  1601,  1601, -2162, -2162,  1332, -2162, -2162,
   -2162, -2162, -2162,  1477,  1332, -2162, -2162,  1948, -2162, -2162,
   -2162,   -32, -2162, -2162, -2162,  6360,  6360,  1521,   318,   240,
    1839,  1525, -2162,  1528, -2162,  5095,  5545, -2162, -2162,  1042,
   -2162,    61,   -26, -2162, -2162, -2162,  1138,  1805,  1976,    83,
   -2162,   142, -2162, -2162, -2162, -2162,   137,  1556,    61, -2162,
   -2162,  6360, -2162, -2162, -2162, -2162,  1603,  1530, -2162, -2162,
   -2162, -2162,  1807,  1042,   535,  1738, -2162,   785, -2162, -2162,
    1418,  7629, -2162,  1766,   645,   783, -2162, -2162,  1042,   783,
    1567, -2162, -2162,    72,  1282, -2162, -2162,  5051, -2162,  1945,
    1741, -2162,   571,  6360,  6360, -2162, -2162,  5342, -2162,  6184,
    1042, -2162,  1806, -2162, -2162,  6360,  7713, -2162, -2162, -2162,
    1880,  1772,  1042, -2162,  1264,    39,   645,  1808,  1295,  1840,
    1699,  1810, -2162,  1414,  1414,  1414, -2162,  1414, -2162,  1570,
   -2162, -2162,  1148, -2162,  1240,  1291, -2162,  6360, -2162, -2162,
   -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162,  1547, -2162,
   -2162,  1500, -2162,  1042,  1572, -2162, -2162, -2162,  1787, -2162,
   -2162, -2162, -2162,  1531,  1535, -2162,  7713,  1787,  1774, -2162,
   -2162, -2162, -2162,   551, -2162,  1699, -2162, -2162,  1332, -2162,
   -2162, -2162, -2162,  1478,  1332, -2162, -2162, -2162, -2162,  1973,
   -2162,  1949, -2162, -2162,   -27, -2162, -2162, -2162, -2162, -2162,
   -2162, -2162,   537, -2162,  1042, -2162,  1261, -2162,  6343,   440,
    6360,    61, -2162,  1654, -2162, -2162, -2162, -2162, -2162, -2162,
   -2162, -2162, -2162, -2162, -2162,  1543,  1688,    47,  1764,  1613,
   -2162,  1559, -2162,  6814, -2162, -2162, -2162, -2162,   639,  1266,
    1629, -2162, -2162, -2162, -2162,  1835, -2162,  1742, -2162,  1042,
   -2162, -2162, -2162, -2162, -2162, -2162,  7629,  7629,  7629,   551,
   -2162, -2162,   551, -2162,  1880,  1880,  1820,  6343,   495, -2162,
    1666, -2162, -2162, -2162, -2162, -2162, -2162, -2162,  2011, -2162,
    1604, -2162, -2162, -2162,   240,  4057, -2162,  6360,   194,   713,
   -2162, -2162,  1744, -2162, -2162,   989, -2162, -2162,   642,   847,
   -2162, -2162, -2162, -2162, -2162, -2162, -2162,  1676,   847, -2162,
   -2162,  7713, -2162,  1979, -2162, -2162, -2162,  6444, -2162,  7713,
    6584, -2162,   134,   -11,   145,    61,    68,  6343,   536,  1295,
    1054, -2162,  1882, -2162, -2162, -2162, -2162,  2027, -2162,  1841,
   -2162, -2162, -2162, -2162, -2162,   405, -2162,  1042,   570,  1071,
    1278,  1573,  1900, -2162,  1574,  7713,  1130, -2162,   537, -2162,
   -2162, -2162,  6360, -2162,  1809,  1785,  1786, -2162,  1789, -2162,
   -2162, -2162, -2162, -2162, -2162,  -156,  1042,  6343,   781,  1615,
    2040,  1042,   922, -2162, -2162, -2162,  1726,  1728, -2162, -2162,
     785,  7713,  1916, -2162, -2162, -2162,  1384,  1863, -2162, -2162,
   -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162,  2004,   847,
   -2162,  1042,  1332, -2162, -2162, -2162, -2162,  1787, -2162, -2162,
   -2162, -2162,   551, -2162, -2162,   551, -2162,  1332,   150,   150,
   -2162, -2162, -2162,  1570, -2162, -2162,  1773,  6343, -2162,    61,
    1628,   535, -2162, -2162,  1764, -2162, -2162, -2162,    47,   802,
   -2162,    12, -2162,  7713,  7713, -2162, -2162,  1856,  1710, -2162,
    1696, -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162,  1987,
    1987,  1987, -2162, -2162, -2162,   551, -2162,  6360, -2162,  6360,
     107,  1042, -2162,    87, -2162, -2162, -2162, -2162, -2162, -2162,
   -2162, -2162, -2162, -2162,  1914, -2162, -2162,  7713,  1913, -2162,
    1356, -2162,  1602,  1286,  1950,  1607,  1638,  7713, -2162, -2162,
    2016, -2162, -2162, -2162, -2162,  6584, -2162,  1876, -2162,  1332,
    1775, -2162, -2162,  1534,    68, -2162,  6360,   415,   310, -2162,
   -2162,  1042,  6360,  1815,   923,  1766, -2162, -2162, -2162,  1917,
    1814, -2162,  1918, -2162,  1819, -2162, -2162, -2162, -2162,  1574,
   -2162, -2162, -2162,  1332,  1071, -2162, -2162, -2162,  1768,  1762,
    1570, -2162,  6360,  1377, -2162,   949, -2162,  7713, -2162,  1964,
    1664, -2162, -2162,   989, -2162,  1191,  2079,  1984,  7713,   -40,
    1332,  1332,  6360,  1572,    67, -2162, -2162, -2162,  1833, -2162,
   -2162,  1629, -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162,
    1764, -2162, -2162, -2162, -2162, -2162, -2162, -2162,    56,  1357,
   -2162,   107,  1357, -2162, -2162,  1042, -2162, -2162, -2162, -2162,
   -2162, -2162, -2162, -2162, -2162, -2162, -2162,  1412,  6343,  1743,
    6343,  1748,   817,   828, -2162,   339,  1582, -2162, -2162, -2162,
   -2162, -2162, -2162,  1946,  1947, -2162, -2162, -2162,  1286, -2162,
    1992, -2162, -2162, -2162,  1377, -2162,  1377,   785, -2162, -2162,
     785,   383,  1042, -2162, -2162,  6343, -2162, -2162,   645,  4750,
   -2162,  2048, -2162,  1844,  1847, -2162,  1848, -2162,   992, -2162,
   -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162,
   -2162,  1827,  1952, -2162,  1954,  1823,  1042,  1629,  6343, -2162,
   -2162, -2162, -2162, -2162,  2040, -2162,  1616,  2008,  1616,  1768,
     612, -2162, -2162,  1953, -2162,  1831, -2162, -2162, -2162,   878,
    1616, -2162, -2162,  1849,  1235,  1414,  1414, -2162, -2162, -2162,
   -2162, -2162, -2162, -2162, -2162,  1428,  1042, -2162, -2162, -2162,
    1975, -2162, -2162,  3541, -2162,   826,  1971, -2162, -2162, -2162,
   -2162, -2162,  6360, -2162,  6360, -2162,    43, -2162, -2162, -2162,
   -2162, -2162, -2162, -2162,  1946, -2162, -2162,  2066, -2162,   785,
   -2162,  1689,  1800,  1054,   814, -2162,  1042, -2162, -2162,  1290,
    6360, -2162, -2162, -2162,  1886, -2162, -2162,  7974, -2162,  7974,
   -2162,  6360,   739, -2162, -2162, -2162, -2162, -2162, -2162, -2162,
   -2162, -2162, -2162, -2162,  1042,  1616, -2162,  1042,  1961, -2162,
   -2162, -2162, -2162, -2162,  1054, -2162, -2162, -2162, -2162, -2162,
   -2162, -2162,  1412,  1042, -2162, -2162,  1812,  1332, -2162, -2162,
   -2162,  1042, -2162, -2162, -2162, -2162, -2162, -2162, -2162,  6360,
   -2162, -2162,  1629,  1629,  -170,   308, -2162,  6360,   785,   785,
   -2162,  6360,  1332,  1054,  1054,  1821, -2162,  1582, -2162,  1760,
    1675,  1332,  1077, -2162, -2162, -2162, -2162, -2162,  1552, -2162,
    1552, -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162,  1042,
   -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162,   535, -2162,
   -2162,  -170, -2162, -2162, -2162,  2012, -2162, -2162,   535,  1054,
    1332, -2162, -2162,  1042,  1977, -2162, -2162,  1829,  1830, -2162,
   -2162, -2162,  2012, -2162, -2162, -2162,   535, -2162, -2162, -2162,
   -2162,  1962,  1963, -2162, -2162,   847,   847, -2162, -2162
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -2162, -2162, -2162, -2162,   125, -2162, -2162, -2162, -2162, -2162,
   -2162, -2162, -2162, -2162, -2162,    55,  1218,  1653,  2084, -2162,
   -2162,  1843, -2162, -2162, -2162,  2110, -2162, -2162,  2111, -2162,
   -2162,  2020,  2069, -2162, -2162, -2162,  2046,   678, -2162, -2162,
   -2162, -2162, -2162, -2162, -2162,  2037, -2162, -2162, -2162, -2162,
    2013, -2162, -2162, -2162, -2162, -2162,   380, -2162, -2162, -2162,
   -2162,  1657, -2162, -2162,  1571,  1335, -2162,  1879, -2162, -2162,
   -2162, -2162, -2162,  1776, -2162, -2162, -2162, -2162, -2162, -2162,
   -2162, -2162,  2125, -2162,  2126, -2162, -2162, -2162, -2162, -2162,
   -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162,
   -2162,  1493,  1216,  1596, -2162,  1690, -2162, -2162, -2162,  1506,
   -2162, -2162, -2162,   -42, -2162,  2136, -2162, -2162,  2134, -2162,
   -2162, -2162, -2162,  1930, -2162, -2162, -2162, -2162, -2162, -2162,
   -2162, -2162, -2162,  2018, -2162, -2162, -2162, -2162, -2162, -2162,
   -2162, -2162, -2162, -2162, -2162, -2162,  1171, -2162, -2162, -2162,
   -2162, -2162,  1333, -2162, -2162, -2162, -2162, -2162, -2162, -2162,
   -2162, -2162, -2162, -2162, -2162, -2162,  1206, -2162, -2162,  1575,
   -2162, -2162,   376,  -636, -2162, -2162, -2162,   617, -2162, -2162,
   -2162, -2162, -2162, -2162, -2162,  -812,  -692,  1195,  -627,   963,
   -2162, -2162,   819, -2162, -2162, -2162,   815, -2162,   180, -2162,
   -2162,   355,  -586, -2162, -2162,  -354, -2162, -2162, -2162,   969,
   -2162,  -366,  -951,   -74, -2162, -2162, -2162, -2162, -2162, -2162,
   -2162, -2162, -2162, -2162,  1590, -2162, -2162, -2162,  1330, -2162,
   -2162, -2162,   881,   118, -2162,    22, -2162, -2162, -2162, -2162,
   -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162,   858,
   -2162, -2162, -2162, -2162, -2162, -2162,   648, -2162, -2162, -2162,
   -2162,   928, -2162, -2162, -2162, -2162, -2162,  1417, -2162, -2162,
   -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162,
    1610,   870, -2162,  -399, -1335, -2162, -2162, -2162, -2162,  1252,
   -2162, -2162, -2162, -2162, -2162, -2162, -2162, -1660, -2162, -2162,
    -267,    -6, -2162,  1185, -2162, -2162, -2162, -1246, -2162, -2162,
   -2162, -2162,   625, -2162, -2162, -2162, -2162, -2162, -2162, -2162,
   -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162,
   -2162, -2162, -2162,    45,   567,    80, -2162, -2162,    81, -2162,
   -2162,  -214, -1542, -2162,   -47,    82, -2162,   -81, -2162, -2162,
   -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162,
   -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162,
   -2162, -2162, -2162, -2162, -2162, -2162, -2162,   289, -2162, -2162,
   -2162, -2162,   801, -2162, -2162, -2162, -2162, -2162, -2162, -2162,
   -2162, -2162, -2162, -2162,   -97, -2162, -2162, -2162, -2162,   519,
   -2162, -2162,   517, -2162, -2162, -2162,   255, -2162,    57, -2162,
   -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162,
   -2162,   787, -2162,   256, -2162, -2162, -2162, -2162, -2162, -2162,
   -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162,
   -2161, -2162, -2162, -2162, -2162, -2162, -2162,  -104,   510, -2162,
   -2162, -2162, -2162, -2162, -2162, -2162,   506, -2162, -2162, -2162,
      33, -2162, -2162,   232, -2162, -2162, -2162, -1572, -2162, -2162,
   -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162,
   -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162,   218,
   -1123,   763, -2162, -2162, -2162,    21,  -381, -2162, -2162, -2162,
   -2162, -2162,    18,  -124, -2162, -2162, -2162, -2162, -2162, -2162,
   -2162, -2162, -2162, -2162, -2162, -2162, -2162,   439, -2162, -2162,
   -2162, -2162, -2162, -2162, -2162,   477, -2162,     8, -2162, -2162,
   -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162,   748,
   -2162,   745, -2162, -2162,   948, -2162, -2162, -2162, -2162, -2162,
   -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162,
   -2162,   464, -2162, -2162, -2162,   742,   181, -2162, -2162, -2162,
   -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162, -2162,
   -2162, -2162, -2162, -2162, -2162,  -139, -2162,   185, -2162, -2162,
   -2162, -2162, -2162, -2162, -2162, -2162,   670, -2162, -2162, -2162,
   -2162, -2162, -2162, -2162, -2162, -2162, -2162,  -166, -2162, -1658,
   -2162, -2162, -1141,   -65, -2162, -1453, -2162, -1618, -2162, -2162,
   -1507, -2162, -2162, -2162, -2162,  -140, -2162, -2162, -2162, -2162,
    -292, -2162, -2162,    32, -2162, -2162, -2162, -2162,  -142, -2162,
   -1860, -2119, -2162, -2162,   329, -2162, -2162, -2162, -2162, -2162,
   -2162, -2162, -2162, -2162,  -797, -1524, -1252, -1638, -2162, -2162,
     626, -1606, -1600, -1597, -2162, -2162, -2162,  1822,  1155,   834,
    4133,  1842, -1470, -1239, -1084,   775,  -202,  -179, -2162, -1297,
   -2162, -1360, -2162,   513,  -778,  -207, -2162,  -689,    -1, -2162,
   -1247, -1330,   971,   868, -1029, -2162,   221, -1396,  -687, -2162,
   -1154,   699, -1219, -2162,   927,   -92,  -421, -1085,   -19, -2162,
    2030,  -183, -2162,  -926,   615,   -89,  1441,  1083, -2162, -2162,
   -2162, -2162, -2162,  1749, -2162,  1542,   691, -2162, -2162, -2162,
   -1697,  -196,  -311,  1579,   561,  -261, -2162, -2162,  -663,  1825,
   -2162,  -981,   276, -1352,  -333,  -638,   129,  -488, -2162, -2162,
   -1128, -2162,  -904, -2162,   -93,  -837,  -361, -1298,  -204,  -895,
     -37,  -993,  -974, -1327, -2162, -2162,  1724,  2001,  1818, -1080,
     599, -2162,   478, -2162,  2179, -2162, -2162, -2162, -1599, -2162,
    1079,   -59, -1068,  -262
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -1798
static const yytype_int16 yytable[] =
{
      93,   178,   265,   251,  1506,   393,   257,   254,   273,   486,
     276,   437,  1403,  1006,   279,   536,  1361,  1911,  1236,  1575,
     848,  1907,  1365,  1912,  1663,  1436,  1913,  1818,  1315,  1092,
    1093,  1095,  1416,   806,   290,   481,   519,   968,  1460,   215,
     216,   217,  1574,   859,   221,  1079,  1647,   226,  1446,  2077,
    1012,  1446,  1002,   359,  1507,   799,   801,  1237,  1584,  1446,
      11,    16,   304,  2425,   399,  2534,  1708,   591,  1446,  1079,
     401,  1970,   358,   254,   198,  1446,   122,  1002,  1810,  1446,
     995,  1492,  1255,   243,   477,   247,   251,   469,   513,   457,
    2022,   458,   459,  2002,   255,   453,  1985,   285,  1003,  2034,
    1770,   185,  1954,   247,   260,   363,  2132,  2118,   229,  1963,
     489,   781,   403, -1797,   569,  1412,  1890,  1720,   270,   271,
     223,   272,  2091,  1245,  1670,   277,   709,   210,   211,  1898,
    1720,    12,   284,   244,   218,  1245,   745,  1865,   457,   606,
     458,   459,   114,  1787,  1589,   175,  2407,  1000,  1079,   997,
    2003,   457,   841,   458,   459,  1442,   349,   453,  1523,  1068,
    1079,  1789,   693, -1797,   453, -1669, -1754,  2292,   317,   852,
     998,   871,  2452,   468,  1368,   453,   395,  1720,   453,  2232,
     818,   398,     4,  1381,   482,  1002,  1776,   457,   488,   458,
     459,   803,  2085,   317,  2291,   763,   274,   453,   274,   308,
    1805,  1257,   274, -1712,  1506,   693,   693,   195,  1775,  2210,
     245,   256,   306,  1496,  1015,  1491,   693,  1318, -1734,  1758,
       3,  1524,   349,   511,  1922,   286,   778,   312,  2026,  1244,
    1645, -1712,  1528, -1797,     9,  1557,  1957,   244,   832,   759,
     188,   274,   453,  1728,   188,  1319,    62,  2111,  1525,   349,
     349,   349,   274,  1648,  1649,  1650,  1773,   320,   274,  -199,
   -1754,   349,  1963,  1759,  1484,   833,   834,   942,  1370,  1667,
   -1728,  2017,   759,  1558,   480, -1712,  1384,  2004,  1891,  1069,
     779,   405,   320,   407,   764, -1712,  2426,  1004,   294,   309,
    1862,   819,   298,  2536,  2211,  1645,   804,   305,   323,  2180,
     491,   317,   483,   476,   230,   326,  -200,   398,  1014,    22,
       5,  1923,   411, -1561,   245,  2177,   123,   307,  1079, -1746,
    1367,  2293,   404,   323,  2294,  2067,  2005,   594,  2190,   479,
     326,  1543,   746,  1036,   812,   782,  1970,   349,   349,  1662,
     349,   349, -1746,  1001,  1070,  1589,   349,   349,   349,   349,
     349,   349,   349,   349,   349,   349,   349,   292,  1553,   813,
    1416,  1464,  1987,  1443,   349,   349,   349,   349,   349,   349,
     349,   349,   349,   349,   681,   683,  1808,   567,   329,   752,
     568,   688, -1669,   693,  2382,  1071,   573,  -199,   578,   579,
     320,  1984,   581,   582,  2030,  1865,  1788,   588,  1083,   454,
    2069,   188, -1561,   329,  1865,  1930, -1712,   710,   999,  2014,
    1763,   457,    25,   458,   459,  1245,  1336,  1594,   416,  1596,
     593,   177,  2266,  2151,  1955,   584,  2281, -1763,   201,  2119,
    2127,   323,   411,  2596,  -200,  2448,   640,    94,   326,   737,
    1079,   247,   248,  1720,   331,  2079,   709,  1646,  2070,  1484,
    2168, -1710, -1712,   453,   783, -1712,  1436,  2133,   681,  1258,
    2107,   454,   411,  2449,   478,   121,  1256,  1416,   454,   331,
     123,   104,  1719,    92, -1734,   308,   665,   592,   334,   454,
     835,   765,   454, -1797,  1777,   851,   989,  1581,  1582,  1668,
     514,  2016,  2309,  2310,   470,    57,  2311,  1729,  2312, -1712,
      92,   454,  1416,   334,  2046,  1838,   455,   753,  1778, -1712,
    2215,   329,  1646,   123,   177,  -656,   177,   177,  1709,  2181,
    1965,   196,  2006,   340,  1966,   177,    87,   716,  2333,   177,
     722,   724,   796,   726,   177,  1970,  1337,   199,   464,    16,
     807,   177,   849,   943,   177,   177,   454,  1964,   340,    16,
     349,   349,   457,  2387,   458,   459,  1416,   823,   349,   274,
    2251,   121,   274,   274,  1730,   309,  1382,  1383,   641,   274,
     457,  2128,   458,   459, -1712,   644,   760,   331,  1031,   607,
     682,   699,   460,   461,   462,   463, -1661,   464,   725,   649,
     457,   823,   458,   459,   465,   460,   461,   462,   463,   776,
     464,  1338,  2165,  2165,  -199,  2196, -1619,   643,   653,   760,
    2462,   334,   453,  2200,   823,   860,   814,  1072, -1712,   101,
    2285,   128,  2422, -1712,  2223,   800,   802,   274, -1712,  1086,
     262, -1712,   809,   462,   463,  2350,   464,  1669,  1031,  -656,
      19,   860,  1340,   864,  2110,   484,   274,  -826,   177,  2240,
     755,  -200,   860,   654,  1865,   836,   340,   457,  2187,   458,
     459,  1416,  2021,   941,   682,  1549,    17,  2182,   946,   274,
     821,  1253,  1254,  1965,  2454,  2357, -1712,  1966,   129,  1705,
    2035,  2183,   957,  1013,   102,   826,  1720,  2450,  1845,  1946,
    1947,  1948,  -654,   244,   840,  1583,   585,  2318,   159,  2320,
   -1739,   845,  2169,   274,   778,   483,    19,  2104,   661,  2475,
    2476,  1816,  2227,  2379,   130,   131,  2108,   132,  2478,   988,
    1682,   856,   857,  1240,   177,   133, -1746,  1035, -1631,  1752,
    2162,   363,  2036,  2163,   865, -1797,   103,    58,   727,   869,
     870, -1763,  1753,  2216,    20,   274,  2355,    59,   715,    64,
     936,  1432,    21,   723,   586,  1433, -1797,   454,   779,  2435,
     123,   614,    21,  2520,  1529,   403,  2165,  2165,  2214,  2146,
     245,  2288,  2526,  2459,  2421,   825,   675,   961,   949,   274,
     962,  2188,  2388,   181,  2042,  1034,   194,    22,  2042,  2055,
     134,  1645, -1631,  2120,  2032, -1712,  2189,    22,  2499,  2286,
    2148,  2345,   274,   274,  1089,  1090,  2451,   256,   987,  2056,
      65,  2441,  1659,  2443,  1094,   671,  -654,   457,  2325,   458,
     459,  2033,  1925,   994,  1831,  2566,   862,   104, -1734,   274,
    2021,  2057,   317,    26,  1098,   203,  2289,   951,   951,  2500,
     274,   457,   493,   458,   459,  2573,  2574,  2501,  2467,  2463,
    -868,   484,  2058,  2228,  1706,   460,   461,   462,   463,   453,
     464,  2395,   158,   350, -1667,  2580,   135,  1659,    23,   194,
      60,  1239,   263,  1530,  1341,  1822,  2561,  2224,  1366,  1078,
    2491,  2492,  2562,   693,  1720,  2563,  2538,   162,  2539,   990,
      25,  2435,   457,  2276,   458,   459,  2277,  1683,  1247,    26,
      25,   136,  1823,  1078, -1619, -1797,    52,  2599,  1659,   662,
      63,   364,   863,  1091,  1613,  1614,   454,  2159,  2160,  2161,
    2502,   320,  2328,  1926, -1712, -1708, -1797,  2267,  2319,   350,
    2321,   495,    24, -1734,   953,   457,  2400,   458,   459, -1739,
    1220,   159,   574,   163,  2557,  1229,  2317,   123,   188,  2325,
    2558,  1232, -1712,  1235,    89,   349,   350,   350,   350,  2597,
     663,  2598,   323, -1619,  1684,   672,  2275,  1241,   350,   326,
    1242,  2325,   496,  1243,    27,   404,    28,  1416,   615,   497,
     188,  2559,  1078,  2377,  2291,   137,  2560,  2446,  2252,  2303,
    2304, -1726,  2424,   177,  1078,   177,   460,   461,   462,   463,
     138,   464, -1040,  -275,   954,   554,   955,   160,  1646,  2292,
     177,   161,  1428,  1416,   460,   461,   462,   463,  1578,   464,
      67,  2544,   101, -1665,   177,  2582,  2583,   128,   139,    53,
     575,  1486,  1355,  2331,   460,   461,   462,   463,   576,   464,
    2059,   177,   329,  1321,   350,   350,   673,   350,   350,  1832,
     256,   537,   499,   350,   350,   350,   350,   350,   350,   350,
     350,   350,   350,   350,  1250,  2101,  1282,   457,  2607,   458,
     459,   350,   350,   350,   350,   350,   350,   350,   350,   350,
     350,  2325,   140,   788,   129,   181,   457,   102,   458,   459,
    1824, -1734,   973,    66,  1419,   666,   457,   227,   458,   459,
    2529,   460,   461,   462,   463,    54,   464,   411,   331,   101,
     648,  2293,  2511,   767,  2412,  1487,  2445,   123,   500,  1490,
     130,   131,  2545,   132,  1509,  2528,   975,  -868,  1593,   991,
     162,   133,  1522,  1825,   256,    69,   977,  1357,  1359,   103,
    1360,  2363,   334,  1357,  1362,    16,   411,  1364,  1989,  1357,
     974,  1990,  1078,  1659,  1991,  1992,  -275,  1562,  2166,    70,
    -275,  2364,  1428,   454,   414,  1747,  2294,   788,   415,  1561,
    1390,  1978,  1755,  1893,   102,  1895, -1712,   961,   456, -1336,
     962, -1734,  1567,  2365,   976,   123,   163,   340,  1449,  1450,
    2512,  2510,   360,  1449,   978,  1463,   134,   503,  1727,  2584,
    2585,   349,   768,  2532,  2366,  2533,  -275,   361,  1563,  1449,
   -1797,  1780,  1368,   778,     4,    17,  1486,   362,  1521,  2227,
     961,   667,   778,   962,  -868,  1532,   103,   164,  1634,   363,
   -1797,  1637,  1638,  1568,  1652,    78,   274,   274,  1552,  1661,
    1554, -1797,  1555,   769,  1510,   190,  2602,  1782,    79,   101,
     770,   969, -1797,  2393,  1222,   992,  1223,   350,   350, -1726,
     195,   460,   461,   462,   463,   350,   464,   779,  2140,  1467,
   -1662,  1774,   135, -1797,  1078,   979,   779,  1489,  -868,    92,
    1489,  1495,  1511,  1467,   256,   460,   461,   462,   463,    94,
     464,    18,  -691,   668,   652, -1712,    71,  1489,  1983,  1990,
    1595,  -868,  1991,  1992,  1933,  1369,  1370,   136,  1461,   487,
    1933,  1407,    19,  1409,   102,   961,   961,   961,   962,   962,
     962,  2279,  2280,   771,   789,  1764,  -275,  2391,  1664,  1593,
    -275,  1520,   193,   980,  -275,  1915,   460,   461,   462,   463,
     203,   464,     5,  2092,    96,   657,  1821,    97,  2419,  1811,
      20,   177,  1313,    72,    73,  1449,   981,  1934,    21,  1355,
    2228,  2093,  1806,  1934,  2401,   274,   103,  1839,  1449,  1220,
    1540,  1541,  1564,  1565,   646,  1829,  1561,  1846,   650,   460,
     461,   462,   463,   655,   464,  -688,   349,  1002,   658,   772,
    1929,   137,  2367,    22,   123,   177,    98,  1569,  1570,  1917,
    -688,  1918,   619,    99,  1664,  1371,   138,  1742,   789,   364,
     114,  1449,  1993,  1994,   982,  1449,    23,    23,  2391,   570,
    2391,   620,   449,   123,  1933,  1792,  1533,   188,  1220,   118,
    1995,  1996,  1224,  1225,   139,  2094,   120,  1807,   961,   207,
     365,   962,  2074,  1101,  1813,  1814,  2075,   366,  1743,   225,
    1815,  2521,   121,  2095,  1982,  2601,  1744,  1817,  1102,   621,
    1819, -1797,  1534,  1919,   123,  2605,  1847,  1848,   773,   315,
     571,  2122,   258,  1495,  1835,  2402,  1707,  1934,  1535,  1835,
      24,    24,   174,  2614,   316,  1843,  2096,    75,   140,  1536,
     872,   873,  2494,   367,  2497,  2334,    25,   730,  1535,   368,
    1849,  1850,  2040,   317,  2097,    26,  2513,   318,    76,  1536,
    2335,   460,   461,   462,   463,   177,   464,  1851,   731,  1852,
     659,  1844,    27,    27,    28,    28,  2149,   181,   369,  1745,
     460,   461,   462,   463,   274,   464,   370,   185,  2233,   660,
     460,   461,   462,   463,  2040,   464,  2082,  2150,   371,   816,
    2548,  1853,  1854,  1855,  1904,   274,  1437,   187,  1438,  2234,
    1993,  1994,  2515,  2049,   777,   274,   188,  2050,   777,   192,
    2516,  2549,  1439,   206,   196, -1710,   203,   372,  1995,  1996,
     595,   319,   596,   188,   718,   373,   719,   533,   534,   622,
     209,  2569,   320,   212,  1949,   543,   493,   222,   547,   548,
     623,  1952,  1678,  1679,  1680,  1681,  1230,  1542,   494,  1856,
     954,  1857,   955,  1942,  1943,  1944,  1945,   223,  1233,  1858,
    1234,   374,  1440,   213,   522,   523,   524,  2122,   219,  1449,
    2087,  2088,  2089,   323,  2090,   324,   823,  1222,   325,  1223,
     326,  1797,  2428,  1798,  2429,   231,  1449,  1613,  1614,  1950,
    2113,  1951,  2114,   327,   810,   233,   811,  2458,   238,  2460,
     234,  2029,   350,  1546,  1547,  2438,  2439,    55,    56,   349,
     256,   328,  2315,  2316,   235,   375,  2043,  1908,   236,  1687,
     259,  1449,  1688,  1617,   237,   495,   261,   267,   268,   352,
     624,   625,   275,   278,  1859,  1689,  1690,  1691,  2065,   288,
     291,   295,   296,   297,  1664,   626,   299,   627,   411,   300,
    2073,   356,   353,   329,  1909,   247,   400,  1982,   409,  1620,
     411,   421,   422,   102,   732,   435,   496,   736,   312,   429,
     738,   739,   740,   497,   430,   742,   743,   744,   432,   747,
     748,   749,   750,   330,   433,  1692,   438,   498,   439,   440,
    1910,  2209,   441,   442,   472,   352,   466,  2217,   443,   474,
     444,   445,   446,   447,  1664,   448,   492,   123,   490,   509,
     518,  1357,   526,   483,   539,  2112,   580,   600,   589,   331,
     645,  2115,   352,   352,   352,   478,   608,   647,   651,   363,
     332,   333,   678,   679,   352,   684,   687,   656,   691,   700,
     701,   703,  2123,  2249,   704,   702,   499,   713,   727,  1449,
    2249,   729,   477,   334,   785,   756,   628,  -364,  2218,   335,
     336,   337,  1693,   794,   274,   795,   797,   820,   808,   824,
     827,   961,   828,   830,   962,   846,   853,   629,   849,   855,
     338,   858,  1694,   866,   876,   938,   867,  2157,   339,   940,
     177,   964,  2354,   947,   349,   349,   349,  1357,   340,  1695,
    1357,   985,   972,  1080,   983,  1081,  1087,  1094,  1096,  1860,
     352,   352,   500,   352,   352,   501,   502,   538,  1100,   352,
     352,   352,   352,   352,   352,   352,   352,   352,   352,   352,
    1103,  1230,  1238,  1246,   860,  1248,  1252,   352,   352,   352,
     352,   352,   352,   352,   352,   352,   352,  1696,   350,  1664,
    1259,  1260,  1291,  1861,  1272,  1307,  1295,  1664,  1664,  1297,
    1385,  1431,  1401,  1449,  1449,  1387,  1697,  1388,  1527,  1544,
    2192,  1550,  1551,  1332,  1698,   693,  1579,  1585,  1602,  2195,
    1597,  1604,  1659,  1653,  1671,  2226,  1714,  1724,  1741,  1703,
    1738,   503,  1754,  1664,  1761,  1699,  1739,  1756,  1757,  1796,
    1768,  1634,  1802,  1766,  1637,  1638,  1800,  1793,  1803,  1673,
    1812,  1827,  1828,  1840,  2250,  1524,  1894,  1900,  1905,  2259,
    2259,  1916,  1901,  1931,  1940,  1953,  2000,  1959,  2015,   961,
    1971,  2024,   962,  2019,  1645,  1709,  2037,  1999,  1974,  2045,
    2052,  2053,  2071,  1476,  2086,  2102,  2066,  2083,  2081,  2274,
    2116,  2105,  1700,  2109,  2106,  2117,  1646,  2130,  2129,  2135,
    1357,  2137,  1701,  1357,  2278,  2140,  2124,  2167,  1934,  2173,
    2171,  2193,  2174,  2186,  2197,  1665,  2221,  1449,  1749,   251,
    2273,  2222,  2236,  2235,  2237,  2245,  2246,  2253,  2243,  2247,
    2254,   961,   961,  2262,   962,   962,  2264,  2268,  2271,  2272,
    2287,  2283,  2307,  2305,  2308,  2313,  2329,  2332,  2341,  2337,
    2344,  2346,  2349,  1357,  2343,  2362,  2371,  2383,  2374,  2376,
    2353,  2386,   274,   352,   352,  2396,  2398,  2408,  2410,  2420,
     347,   352,  1702,   350,  2442,   961,  2469,  2228,   962,  2444,
    2335,  2453,  2456,  2470,  2471,  1664,  1782,  2472,  2473,  2480,
    2488,  1665,  2495,  1664,  2483,  2402,  2352,  2486,  2530,  2507,
    2523,  2537,  1449,  2514,  2540,  2551,  2571,  2542,  2591,  2029,
    2592,  1397,  2603,  2611,  2612,  2609,   996,  2615,  2616,  2577,
      91,    82,  1426,  1430,    83,  2465,   208,   126,  2590,   426,
    2381,   191,  2326,   202,  2047,   228,   347,  1470,  1472,   861,
     402,  2390,   590,   690,    84,  1664,   758,    85,   670,  1018,
     577,   751,   508,  1516,  1397,  2356,   961,  2415,  2416,   962,
      86,    88,   355,   347,   347,   347,   240,  1097,   708,  1085,
     952,  1339,  1576,  1577,  2213,   347,  2100,  1342,  2504,  2418,
     706,   967,  1539,  2613,  2270,  1556,  2427,  1830,   842,   698,
    1019,  1896,  2555,  2437,  2359,  1219,  1866,  1921,  2290,  2527,
    2296,  2436,  2413,  2302,  2154,  1674,  2447,  1962,  1973,  2176,
    2327,  1717,  2518,  2519,  2455,  2185,  1998,  2011,  2348,  2198,
    2219,  1748,  2360,  2368,  2474,  2080,  2044,  2380,  1772,  1771,
    1508,  2062,  2390,  2260,  2390,  2493,  1791,  2261,  1863,  2498,
    2595,  2543,  2373,  2509,  1601,  1914,  2155,  1765,  2031,  2241,
    1837,   347,   347,  1644,   347,   347,   613,  1519,   555,   354,
     347,   347,   347,   347,   347,   347,   347,   347,   347,   347,
     347,  1572,  1677,   817,  2490,  1903,   798,  2013,   347,   347,
     347,   347,   347,   347,   347,   347,   347,   347,  2575,   686,
     963,   427,  1718,   583,  1977,   220,  2099,  1357,  1363,     0,
       0,     0,     0,  1723,     0,   251,     0,     0,     0,     0,
       0,     0,     0,   274,  2522,   604,  1737,     0,     0,     0,
       0,     0,  2589,     0,     0,     0,     0,     0,     0,     0,
     880,   881,   882,   883,   884,   885,   886,     0,     0,     0,
       0,     0,     0,     0,   251,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   350,     0,  1786,     0,
       0,     0,  1516,     0,     0,     0,     0,  1601,     0,     0,
       0,   888,     0,   889,   890,   891,   892,   893,   894,   895,
       0,     0,  2568,     0,     0,  2570,     0,     0,     0,     0,
     251,  1665,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  2576,     0,     0,  2578,     0,     0,  2535,     0,  2579,
       0,     0,     0,     0,     0,     0,   896,   897,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  2547,     0,  2588,
       0,     0,     0,     0,     0,     0,     0,     0,  2593,     0,
       0,     0,     0,     0,     0,     0,     0,  1899,     0,     0,
       0,  1665,     0,     0,     0,     0,     0,  2600,     0,     0,
       0,     0,     0,     0,   347,   347,     0,     0,   352,     0,
       0,     0,   347,     0,     0,     0,     0,  2606,     0,     0,
       0,  2608,     0,     0,     0,     0,     0,  1644,     0,     0,
       0,  1456,     0,     0,     0,     0,     0, -1460,     0,     0,
   -1460,     0,     0, -1460, -1460, -1460,     0,     0,     0,   898,
       0, -1460,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0, -1460, -1460,
   -1460, -1460,     0,     0,     0,     0,  2617,  2618,     0,     0,
       0,   350,   350,   350,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1956,  1958,     0,     0,     0,     0,     0,
       0,     0,   901,     0,  1981,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0, -1460,     0,     0,     0,     0,     0,     0,  2018,
       0,     0,     0,     0,     0,     0,  1665, -1460,     0,   903,
       0,     0,     0,     0,  1665,  1665,     0,   905,     0,     0,
       0,     0,     0,     0,     0,     0, -1460,     0,     0,     0,
   -1460,   906,     0,     0,     0,  2051,     0,     0,     0,     0,
       0,  2060,  2061,     0,     0,  1786,     0,  2064,     0,     0,
    1665,     0,     0,  2068,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0, -1460, -1460,     0,     0,     0,
       0,     0,     0,     0,     0,  2098,     0,     0, -1460,     0,
       0,   909,   910,   911,     0,     0,     0,     0,     0,     0,
     963,     0,     0,     0, -1460, -1460,     0,     0,     0,     0,
       0, -1460,     0,     0,     0, -1460,     0,     0,  1465,     0,
       0,     0,     0,     0,   352,     0,     0,     0,     0,     0,
       0,     0,     0,     0, -1460,     0,     0,     0,     0,     0,
       0,     0, -1460,   963,     0, -1460,     0, -1460,     0,   913,
     914,   915, -1460,     0,     0,     0, -1460,     0, -1460,     0,
       0, -1460,     0, -1460,     0,     0,     0,  1391,     0,     0,
    1002,  1084,     0,     0,     0,     0, -1460,     0, -1460, -1460,
       0,     0, -1460,     0,     0,     0,     0,     0,     0,     0,
       0,     0, -1460,     0, -1460,     0,     0,     0,     0, -1460,
       0,     0,  1665,     0, -1460,     0,     0,     0,     0,     0,
    1665,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0, -1460,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  2179, -1460,     0,   963,   963,
     963,     0,     0,     0,     0,     0,     0,     0,     0,  -651,
    1020,  1666,   315,  -700,     0,  -700,     0,     0,     0,     0,
    -700,     0,  1665,     0,     0,     0, -1460,   316,  -700,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   347,
       0,     0,     0,     0,     0,     0,   317,     0,     0,     0,
     318,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0, -1460,     0,     0,     0,  -700,  -700,     0,   352,
    2242,     0,  -700, -1460, -1460,     0, -1460,  -700,     0,  -700,
       0,     0,     0,     0,     0,     0,     0,  1666,     0,     0,
    -700,     0,     0,  -700,     0,  1655, -1460,     0,     0,     0,
       0,     0, -1460, -1460, -1460,     0,  -700,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   963,     0, -1460,   319,     0,     0,     0,  -700,     0,
       0, -1460,     0, -1460,     0,   320,     0,  -700,     0,     0,
    -700, -1460, -1460, -1460, -1460, -1460,     0, -1460,     0, -1460,
   -1460, -1460, -1460, -1460,     0,     0,     0,     0,     0,     0,
       0,     0,  -651,     0,     0,  1393,  -651,     0,     0,     0,
       0,     0,   322,     0,     0,     0,   323,  -700,   324,     0,
       0,   325,     0,   326,     0,     0,     0,  1897,     0,  -700,
       0,     0,     0,  -700,  -700,     0,   327,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -700,     0,
       0,     0,  -651,     0,   328,  -700,     0,     0,  -700,  -700,
       0,     0,     0,     0,     0,     0,  -700,     0,     0,     0,
       0,     0,     0,  -700,     0,  -700,     0,     0,  -700,     0,
    2361,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   329,     0,     0,     0,
       0,     0,  1420, -1797,     0, -1797,     0,     0, -1797,     0,
       0,     0,     0,     0,     0,  -700,     0,     0,     0,  -700,
       0,  -700,     0, -1797,     0,   347,   330,     0,     0,  -700,
    2417,     0,     0, -1165,     0,     0,     0, -1165, -1165, -1165,
   -1165, -1165, -1165, -1165, -1165, -1165,     0, -1708, -1165, -1165,
    1749,  -700, -1165, -1165, -1165, -1165, -1165, -1165, -1165, -1165,
   -1165, -1165,   331,     0,     0,  -700,     0,     0,     0,     0,
       0, -1797,   518,   332,   333,     0,     0,     0,     0,     0,
    -651,     0,     0,     0,     0,     0,     0,     0,  -700,  -700,
       0,     0,   352,     0,     0,     0,   334,     0,     0,  -700,
       0,     0,   335,   336,   337,     0,     0,     0, -1797,     0,
       0,  -700,  -700,     0,     0,     0,     0,     0,  -700,     0,
    -700,     0,     0,   338,     0,     0,     0,  1666,  -700,     0,
       0,   339,     0,   177,     0,     0,     0,     0,  -700,     0,
       0,   340,     0,     0,     0,     0,     0,  -700, -1797,  1656,
       0,     0,     0,     0,     0,     0,  -700,     0,     0,     0,
    -700,  -700,     0,     0,  -700,     0,     0,  -700, -1797,     0,
       0,  1587, -1797,  -700,     0,     0,     0, -1165, -1165, -1165,
   -1165, -1165, -1165,     0,     0, -1797,  1020,  1666,     0,  -700,
    -700,  -700,     0,     0,     0,     0,  -700,     0,  -700,     0,
       0,     0,     0,  -700,  -700,     0,     0,     0,     0,     0,
     347,     0,     0,     0,  -700,     0,     0,     0,  1867,     0,
    1868,     0,  1021,  1869, -1708,  -700,     0,  -700,  2550,     0,
       0,     0, -1797, -1797,     0,     0,     0,  1022,  1870,  2556,
       0,     0,  -700,  -700,     0,     0,     0,     0,  -700,     0,
       0,     0, -1797,  -700,   963,  -700,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -700,     0,     0,  -700,
       0,     0,     0,     0,     0,     0,     0,   352,   352,   352,
       0, -1797,  -700,     0,     0,     0,  1871,  2581,     0,     0,
       0, -1708,     0,     0,     0,  2586,     0,     0,     0,  2587,
       0,     0,     0,     0,  -700, -1797,     0,     0,     0,     0,
       0,     0,     0,  -700,     0,     0,  -700,     0,     0,     0,
       0,     0,     0,  1872,     0,     0,     0,  1099,     0,     0,
       0, -1797,  1666,     0,     0,     0,     0,     0, -1797,     0,
    1666,  1666,     0,     0,     0,     0,     0,     0, -1797,     0,
       0,     0, -1797,  -700,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1873, -1165,  -700,     0,     0,     0,  -700,
    -700,     0,     0,     0,     0,     0,  1666,     0,     0,     0,
       0,     0, -1165,  1874,  -700,     0,     0,  1875,     0,     0,
       0,  -700, -1797,     0,  -700,  -700,     0,     0,     0,     0,
    1876,  1588,  -700,     0,     0,     0,     0,     0,     0,  -700,
       0,  -700,   963,     0,  -700,     0,     0,     0, -1797,     0,
       0,     0,     0,     0,     0,     0,     0,     0, -1797,     0,
       0,     0,     0,     0, -1797,     0,     0,     0,  -868,     0,
       0,  -868,     0,   123,     0,     0,     0,  1877,  1878,     0,
       0,  -700,     0,     0,     0,  -700,     0,  -700,     0,     0,
       0,     0,     0,     0,     0,  -700,     0,  1879,     0,     0,
       0,     0,     0,     0,   963,   963,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   693,  -700,     0,     0,
       0,     0,     0,   347,     0,     0,  1880,     0,     0,     0,
       0,  -700,     0,     0,     0,     0,     0,     0,   518,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   963,     0,
    1881,     0,     0,  -868,  -700,  -700,     0, -1712,  1666,     0,
       0,     0,     0,     0,     0,  -700,  1666,  1391,  -868,     0,
    1002,     0,     0,  1605,  1606,  1607,  1882,  -700,  -700,     0,
       0,  1608,     0,  1883,  -700, -1712,  -700,  -868,     0,     0,
       0,  -868,     0,  1884,  -700,     0,     0,  1885,  1609,  1610,
    1611,  1612,     0,     0,  -700,     0,     0,     0,     0,     0,
       0,     0,     0,  -700,  2392,     0,     0,     0,  1666,     0,
       0,     0,  -700,     0,     0,     0,  -700,  -700,     0,   963,
    -700,     0,     0,  -700,     0,     0,     0,  1886,     0,  -700,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   315,     0,     0,     0,  -700,     0,     0,     0,
       0,     0,     0,  1887,  -700,  -868,     0,   316,     0,  -700,
       0,     0,     0,  1888,     0,     0,  -868,     0,     0,  1889,
    -700,     0,     0,     0,     0,     0,   317,     0,  1021,     0,
     318,  -700,     0,  -700,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1022,     0,  2392,  -868,  2392,   347,   347,
     347,     0,     0,  -868,     0,     0,     0,  -868,     0,  -868,
       0,     0,  -868,     0,  -868,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1613,  1614,  -868,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1615,     0,
       0,     0,     0,     0,     0,  -868,     0,     0,     0,     0,
    -868,     0,     0,     0,   319,  1616,     0,     0,     0,     0,
       0,  1617,     0,     0,   313,   320,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1618,     0,     0,  -868,     0,     0,
       0,     0,  1619,   314,     0,  1393,     0,  1620,     0, -1712,
       0,     0,   322,     0,     0,     0,   323,     0,   324,     0,
       0,   325,     0,   326,     0,     0,     0,  -868,     0,     0,
       0,     0,     0,     0,     0,     0,   327,     0,  1621,  1622,
       0,     0,  1623,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1624,     0,   328,     0,   315,  -868,     0,  1625,
       0,     0,     0,  -868,  1626,     0,     0,     0,     0,     0,
       0,   316,     0,     0,  -868,  -868,     0,     0,     0,     0,
       0,  1627,     0,     0,  1104,     0,  1105,     0,     0,     0,
     317,  1106,     0,     0,   318,     0,   329,  -868,     0,  1107,
       0,     0,     0,  -868,  -868,  -868,     0,     0,     0,     0,
       0,  -868,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -868,     0,   330,     0, -1712,     0,
       0,     0,  -868,     0,  -868,     0,     0,  1108,  1109,     0,
       0,     0,  -868,  1110,     0,     0,     0,     0,  1111,     0,
    1112,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1113,   331,     0,  1114,     0,     0,     0,   319,     0,
       0,     0,     0,   332,   333,     0,  1628,  1115,     0,   320,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   334,     0,     0,  1116,
       0,     0,   335,   336,   337,     0,     0,     0,  1117,   321,
       0,  1118,     0,     0,     0,     0,   322,     0,     0,     0,
     323,     0,   324,   338,     0,   325,     0,   326,     0,     0,
       0,   339,     0,   177,     0,     0,     0,     0,     0,     0,
     327,   340,  1629,  1630,  1631,  1632,     0,  1633,  1119,  1634,
    1635,  1636,  1637,  1638,     0,     0,     0,     0,   328,     0,
    1120,     0,     0,     0,  1121,  1122,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1123,
       0,     0,     0,     0,     0,     0,  1124,     0,     0,  1125,
    1126,     0,     0,     0,     0,     0,     0,  1127,     0,     0,
     329,     0,     0,     0,  1128,     0,  1129,     0,     0,  1130,
       0,     0,     0,     0,     0,     0, -1001,     0,     0,     0,
   -1001, -1001, -1001, -1001, -1001, -1001, -1001, -1001, -1001,     0,
     330, -1001, -1001, -1001,     0, -1001, -1001, -1001, -1001, -1001,
   -1001, -1001, -1001, -1001, -1001,     0,  1131,     0,     0,     0,
    1132,     0,  1133,     0,     0,     0,     0,     0,     0,     0,
    1134,     0,     0,     0,     0,     0,   331,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   332,   333,     0,
       0,     0,  1135,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1136,     0,     0,     0,
     334,     0,     0,   518,     0,     0,   335,   336,   337,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1137,
    1138,     0,     0,     0,     0,     0,     0,   338,     0,     0,
    1139,     0,     0,     0,     0,   339,     0,   177,     0,     0,
       0,     0,  1140,  1141,     0,   340,   341,   342,     0,  1142,
       0,  1143,     0,     0,   343,     0,     0,     0,     0,  1144,
   -1001, -1001, -1001, -1001, -1001, -1001,     0,     0,     0,  1145,
       0,     0,     0,     0,     0,     0,     0,     0,  1146,     0,
       0,     0,     0,     0,     0,     0,     0,  1147,     0,     0,
       0,  1148,  1149,     0,     0,  1150,     0,     0,  1151,     0,
       0,   346,     0,     0,  1152,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1153,     0,     0,     0,     0,     0,     0,     0,  1154,
       0,     0,     0,     0,  1155,     0,     0,  1002,     0,     0,
       0,     0,     0,     0,     0,  1156,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1157,     0,  1158,  1037,
       0,  1038,     0,     0,  1039,     0,     0,   428,  1159,   880,
     881,   882,   883,   884,   885,   886,   887,  1040,  1041,  1042,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   450,   451,   452,     0,     0,     0,
       0,     0,     0,  1043,     0,     0,   467,     0,     0,     0,
     888,     0,   889,   890,   891,   892,   893,   894,   895,     0,
       0,     0,     0,     0,     0,  1044,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1391,     0,     0,
    1002,     0,     0,     0,     0,     0,     0, -1001,     0,     0,
       0,     0,     0,   317,     0,   896,   897,     0,     0,     0,
       0,  1420,     0,     0,     0, -1001,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   532,     0,     0,     0,     0,     0,     0,
       0,   540,   542,     0,   544,   546,     0,     0,   549,   551,
     552,   553,     0,     0,  1045,     0, -1708,     0,     0,   556,
     557,   558,   559,   560,   561,   562,   563,   564,   566,     0,
       0,     0,   315,     0,  1046,     0,     0,  1047,  1048,     0,
       0,     0,     0,     0,     0,     0,     0,   316,     0,     0,
       0,  1049,   320,     0,     0,     0,     0,     0,   898,     0,
       0,     0,     0,     0,     0,     0,   317,     0,     0,     0,
     318,     0,     0,     0,   899,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   900,     0,     0,     0,     0,
    1050,     0,     0,   323,     0,     0,     0,     0,  1051,  1052,
     326,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   901,     0,     0,     0,     0,     0,     0,  1053,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1054,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   319,     0,     0,  1055,   903,     0,
       0,     0,     0,     0,   904,   320,   905,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     906,  1056,     0,   329,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1104,  1393,  1105,     0,     0,     0,
       0,  1106,   322, -1708,     0,     0,   323,  1057,   324,  1107,
       0,   325,     0,   326,  1058,   733,   735,     0,     0,     0,
       0,     0,     0,   741,     0,     0,   327,     0,  1059,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   219,     0,
     909,   910,   911,  1060,   328,     0,     0,  1108,  1109,   331,
       0,     0,     0,  1110,     0,     0,     0,     0,  1111,     0,
    1112,     0,     0,     0,     0,     0,     0,     0,  1061,     0,
   -1708,  1113,     0,     0,  1114,     0,     0,     0,     0,  1062,
       0,     0,     0,   334,     0,     0,   329,  1115,     0,     0,
       0,     0,     0,     0,  1063,     0,     0,     0,   913,   914,
     915,     0,     0,     0,     0,     0,     0,     0,     0,  1116,
    1064,   916,     0,  1065,  1066,     0,   330,     0,  1117,     0,
     177,  1118,     0,     0,     0,     0,     0,     0,   340,     0,
       0,     0,     0,     0,     0,  1067,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   331,     0,     0,     0,     0,     0,  1119,     0,
       0,     0,     0,   332,   333,     0,     0,     0,     0,     0,
    1120,     0,     0,     0,  1121,  1122,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   334,     0,     0,  1123,
       0,     0,   335,   336,   337,     0,  1124,     0,     0,  1125,
    1126,     0,  1391,     0,     0,  1002,     0,  1127,  1421,     0,
       0,     0,     0,   338,  1128,     0,  1129,     0,     0,  1130,
       0,   339,   123,   177,     0,     0,  1420,     0,     0, -1797,
       0,   340, -1797,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0, -1797, -1797, -1797,     0,     0,
       0,     0,     0,     0,     0,     0,  1131,     0,     0,     0,
    1132,     0,  1133,     0,     0,     0,     0,     0,     0,     0,
    1134, -1708,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   315,     0,     0,
       0,     0,  1135,     0,     0, -1797,     0,     0,  1391,     0,
       0,  1002,   316,     0,     0,     0,  1136,     0,     0,     0,
       0,     0,     0,   518,     0,     0,     0,     0,     0,     0,
       0,   317,     0,     0,     0,   318,     0,     0,     0,  1137,
    1138,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1139,     0, -1769,     0,     0, -1769,     0,     0,     0,     0,
       0,     0,  1140,  1141,     0,     0,     0,     0,     0,  1142,
       0,  1143,     0,     0,     0,     0,     0,     0,     0,  1144,
       0,     0, -1797,     0,     0,     0,     0,  1673,     0,  1145,
       0,     0,     0,   315,     0,     0,     0,     0,  1146,     0,
       0,     0, -1797,     0,     0,     0,     0,  1147,   316,   319,
       0,  1148,  1149,     0,     0,  1150,     0,     0,  1151, -1797,
     320,     0,     0,     0,  1152,     0,     0,   317,     0,     0,
       0,   318,     0,     0,     0,     0,     0, -1769,     0,     0,
       0,  1153,     0,     0,     0,     0,     0,     0,     0,  1154,
    1393,     0, -1769,     0,  1155,     0,     0,   322, -1708,     0,
       0,   323,     0,   324,     0,  1156,   325, -1797,   326,  2048,
       0, -1769,     0,     0,     0, -1769,  1157,     0,  1158,     0,
       0,   327,     0,     0,     0,     0, -1797,     0,     0,     0,
       0,     0,     0,     0, -1797,     0,     0,     0,     0,   328,
       0,     0,     0,     0,     0,   319,     0,     0,     0,     0,
       0,  1391,     0,     0,  1002, -1797,   320,     0,     0,     0,
       0,     0,     0,     0,     0, -1708,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   329,     0,     0,     0,     0,  1393,     0,     0, -1769,
       0,     0,  1452,   322,     0,     0,     0,   323,     0,   324,
   -1769,     0,   325,     0,   326,     0,     0,     0,     0,  1442,
       0,   330, -1797,     0,     0,     0,     0,   327,     0,     0,
       0,     0, -1797,     0,     0,     0,     0,     0,     0,     0,
   -1769,     0,     0,     0,     0,   328,   315, -1769,     0,  1391,
       0, -1769,  1002, -1769,     0,     0, -1769,   331, -1769,     0,
       0,   316,     0,  1391,     0,     0,  1002,     0,   332,   333,
       0, -1769,     0,     0,     0,     0,     0,     0,     0,     0,
     317,     0,     0,     0,   318,     0,     0,   329,     0, -1769,
       0,   334,     0,     0,     0,     0,     0,   335,   336,   337,
       0,     0, -1797,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1421,     0,     0,     0,   330,   338,     0,
       0,     0,     0,     0,     0,     0,   339,   123,   177,     0,
       0, -1769,     0,     0,   315,     0,   340,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   315,   316,
       0,     0,     0,   331,     0,     0,     0,     0,   319,  1453,
       0, -1769,     0,   316,   332,   333,     0,     0,   317,   320,
       0,     0,   318,     0,     0,     0,  1513,     0,     0,     0,
       0,     0,   317,     0,  2049,     0,   318,   334,  2050,     0,
       0,     0,     0,   335,   336,   337,     0, -1769,     0,  1393,
       0,     0,     0,     0,     0,     0,   322,     0, -1769, -1769,
     323,     0,   324,     0,   338,   325,     0,   326,     0,     0,
       0,     0,   339,     0,   177,     0,     0,  1443,     0,     0,
     327, -1769,   340,     0,     0,     0,     0, -1769, -1769, -1769,
       0,     0,     0,     0,     0,     0,   319,     0,   328,  1783,
       0,     0,  1391,     0,     0,  1002,     0,   320, -1769,     0,
     319,     0,     0,     0,     0,     0, -1769,     0, -1769,     0,
       0,   320,     0,     0,     0,     0, -1769,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1790,  1393,     0,     0,
     329,     0,     0,     0,   322,     0,     0,     0,   323,     0,
     324,  1393,     0,   325,     0,   326,     0,     0,   322,     0,
       0,     0,   323,     0,   324,     0,     0,   325,   327,   326,
     330,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   327,     0,     0,     0,   328,   315,  1391,     0,
       0,  1002,     0,     0,     0,     0,     0,     0,     0,     0,
     328,     0,   316,     0,     0,     0,   331,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   332,   333,     0,
       0,   317,     0,     0,     0,   318,     0,     0,   329,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     334,     0,   329,     0,     0,     0,   335,   336,   337,     0,
       0,     0,     0,     0,     0,     0,  1784,     0,   330,     0,
       0,     0,     0,     0,     0,     0,     0,   338,     0,     0,
       0,     0,   330,   315,     0,   339,  1391,   177,     0,  1002,
       0,     0,     0,     0,  1392,   340,     0,     0,   316,     0,
       0,     0,     0,     0,   331,     0,     0,     0,     0,   319,
       0,     0,     0,     0,     0,   332,   333,   317,   331,     0,
     320,   318,     0,     0,     0,     0,     0,     0,     0,   332,
     333,     0,     0,     0,     0,     0,     0,     0,   334,     0,
       0,     0,     0,     0,   335,   336,   337,     0,     0,     0,
    1393,     0,   334,     0,     0,     0,     0,   322,   335,   336,
     337,   323,     0,   324,     0,   338,   325,     0,   326,     0,
       0,   315,     0,   339,  1391,   177,     0,  1002,     0,   338,
       0,   327,  1468,   340,     0,     0,   316,   339,     0,   177,
       0,     0,     0,     0,     0,   319,     0,   340,     0,   328,
       0,     0,     0,     0,     0,   317,   320,     0,     0,   318,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1393,     0,     0,     0,
       0,   329,     0,   322,     0,     0,     0,   323,     0,   324,
       0,     0,   325,     0,   326,     0,     0,     0,     0,   315,
    1391,     0,     0,  1002,     0,     0,     0,   327,     0,     0,
       0,   330,     0,     0,   316,     0,     0,     0,     0,     0,
       0,     0,     0,   319,     0,   328,     0,  1513,     0,     0,
       0,     0,     0,   317,   320,     0,     0,   318,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   331,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   332,   333,
       0,     0,     0,     0,  1393,     0,     0,   329,     0,     0,
       0,   322,     0,     0,     0,   323,     0,   324,     0,     0,
     325,   334,   326,     0,     0,   315,     0,   335,   336,   337,
       0,     0,     0,     0,     0,   327,  1517,   330,     0,     0,
     316,     0,     0,  1978,     0,     0,     0,     0,   338,     0,
       0,   319,     0,   328,     0,     0,   339,   123,   177,   317,
       0,     0,   320,   318,     0,     0,   340,     0,     0,     0,
       0,     0,     0,   331,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   332,   333,     0,     0,     0,     0,
       0,     0,  1393,     0,     0,   329,     0,     0,     0,   322,
       0,     0,     0,   323,     0,   324,     0,   334,   325,     0,
     326,     0,     0,   335,   336,   337,     0,     0,     0,     0,
       0,     0,     0,   327,     0,   330,     0,     0,     0,  1391,
       0,     0,  1002,     0,   338,     0,     0,   319,     0,     0,
       0,   328,   339,  1391,   177,     0,  1002,     0,   320,     0,
       0,     0,   340,     0,     0,     0,     0,     0,     0,     0,
       0,   331,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   332,   333,     0,     0,     0,     0,  1393,     0,
       0,     0,     0,   329,     0,   322,     0,     0,     0,   323,
       0,   324,     0,     0,   325,   334,   326,     0,     0,     0,
       0,   335,   336,   337,     0,     0,     0,     0,     0,   327,
       0,     0,     0,   330,   315,     0,     0,     0,     0,     0,
       0,     0,   338,     0,     0,     0,     0,   328,   315,   316,
     339,  1391,   177,     0,  1002,     0,     0,     0,     0,     0,
     340,     0,     0,   316,     0,     0,     0,     0,   317,   331,
       0,     0,   318,     0,     0,     0,     0,     0,     0,     0,
     332,   333,   317,     0,     0,     0,   318,     0,     0,   329,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   334,     0,     0,     0,     0,     0,   335,
     336,   337,     0,     0,     0,     0,     0,     0,     0,   330,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     338,     0,     0,     0,     0,     0,   315,     0,   339,     0,
     177,     0,     0,     0,     0,     0,   319,     0,   340,     0,
       0,   316,     0,     0,     0,   331,  1795,   320,     0,     0,
     319,     0,     0,     0,     0,     0,   332,   333,     0,     0,
     317,   320,     0,     0,   318,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1393,     0,   334,
       0,     0,     0,     0,   322,   335,   336,   337,   323,     0,
     324,  1393,     0,   325,     0,   326,     0,     0,   322,     0,
    1391,     0,   323,  1002,   324,     0,   338,   325,   327,   326,
       0,     0,     0,     0,   339,     0,   177,  1391,     0,     0,
    1002,     0,   327,     0,   340,     0,   328,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   319,     0,
     328,     0,     0,     0,     0,     0,     0,     0,     0,   320,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   329,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1393,
       0,     0,   329,     0,     0,   315,   322,     0,     0,     0,
     323,     0,   324,     0,     0,   325,     0,   326,   330,     0,
     316,     0,   315,     0,  2002,     0,     0,     0,     0,     0,
     327,     0,   330,     0,     0,     0,     0,   316,     0,   317,
       0,     0,     0,   318,     0,     0,     0,     0,   328,     0,
       0,     0,     0,     0,   331,     0,   317,     0,     0,     0,
     318,     0,     0,     0,     0,   332,   333,     0,   331,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   332,
     333,  2003,     0,     0,  1599,     0,     0,     0,   334,     0,
     329,     0,     0,     0,   335,   336,   337,     0,     0,     0,
       0,     0,   334,     0,     0,     0, -1121,     0,   335,   336,
     337,     0,     0,     0,     0,   338,     0,   319,     0,     0,
     330, -1121,     0,   339,     0,   177,     0,     0,   320,   338,
       0,     0,     0,   340,   319,     0,     0,   339,     0,   177,
   -1121,     0,     0,     0, -1121,   320,     0,   340,     0,     0,
    2063,     0,     0,     0,     0,     0,   331,     0,  1393,     0,
       0,     0,     0,     0,  2201,   322,     0,   332,   333,   323,
       0,   324,     0,     0,   325,  1393,   326,     0,     0,     0,
       0,     0,   322,     0,     0,     0,   323,     0,   324,   327,
     334,   325,     0,   326,     0,     0,   335,   336,   337,     0,
       0,     0,     0,     0,     0,     0,   327,   328,  2004,     0,
       0,     0,     0,     0,     0,     0,     0,   338, -1121,     0,
       0,  2202,     0,     0,   328,   339,     0,   177,     0, -1121,
       0,     0,     0,     0,     0,   340,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   315,     0,     0,   329,
       0,     0,     0,     0,     0,     0,     0,  2005,     0,     0,
       0,   316,     0,     0,     0,     0,   329,     0,     0,     0,
   -1121,     0, -1121,     0,     0, -1121,     0, -1121,     0,   330,
     317,     0,     0,     0,   318,     0,     0,     0,     0,     0,
   -1121,     0,     0,     0,   314,     0,   330,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0, -1121,     0,
    2124,     0,     0,     0,     0,   331,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   332,   333,     0,     0,
       0,     0,   331,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   332,   333,     0,     0,   315,     0,   334,
   -1121,     0,     0,     0,     0,   335,   336,   337,   319,     0,
       0,     0,   316,     0,     0,     0,   334,     0,     0,   320,
       0,     0,   335,   336,   337,     0,   338,     0,     0,     0,
   -1121,   317,     0,     0,   339,   318,   177,     0,     0,     0,
       0,     0,     0,   338,   340,     0,     0,  2203,     0,     0,
       0,   339,     0,   177,     0,     0,     0,   314,     0,     0,
     323,   340,   324,     0,     0,   325, -1121,   326,     0,     0,
       0,     0,     0,     0,     0,     0,     0, -1121, -1121,     0,
     327,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   328,     0,
   -1121,     0,     0,  2006,     0,     0, -1121, -1121, -1121,   319,
     315,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     320,     0,     0,     0,     0,   316,   315, -1121,     0,     0,
       0,     0,     0,     0,     0, -1121,     0, -1121,     0,     0,
     329,   316,     0,     0,   317, -1121,     0,     0,   318,     0,
     321,     0,     0,     0,     0,     0,     0,   322,     0,     0,
     317,   323,     0,   324,   318,     0,   325,     0,   326,     0,
     330,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   327,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   314,     0,     0,     0,     0,     0,     0,     0,   328,
       0,     0,     0,     0,     0,     0,   331,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   332,   333,     0,
       0,     0,   319,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   320,     0,     0,     0,     0,   319,     0,
     334,   329,     0,  2204,   315,     0,   335,   336,   337,   320,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   316,
       0,     0,     0,   321,     0,     0,     0,   338,     0,     0,
     322,   330,     0,     0,   323,   339,   324,   177,   317,   325,
       0,   326,   318,     0,     0,   340,     0,     0,     0,     0,
     323,     0,   324,     0,   327,   325,     0,   326,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   331,     0,     0,
     327,     0,   328,     0,     0,     0,     0,     0,   332,   333,
       0,     0,     0,     0,     0,     0,     0,     0,   328,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   334,     0,     0,     0,     0,     0,   335,   336,   337,
       0,     0,     0,     0,   329,     0,   319,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   320,   338,     0,
     329,     0,     0,     0,     0,     0,   339,     0,   177,     0,
       0,     0,     0,     0,   330,     0,   340,   341,   342,     0,
       0,     0,     0,     0,     0,   343,   565,   321,  2143,     0,
     330,     0,     0,     0,   322,     0,     0,     0,   323,     0,
     324,     0,     0,   325,     0,   326,     0,     0,     0,     0,
     331,     0,     0,     0,     0,     0,     0,     0,   327,     0,
       0,   332,   333,     0,     0,     0,   331,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   328,   332,   333,     0,
       0,     0,     0,     0,   334,     0,     0,     0,     0,     0,
     335,   336,   337,     0,     0,     0,     0,     0,     0,     0,
     334,     0,     0,     0,     0,     0,   335,   336,   337,     0,
       0,   338,     0,     0,     0,     0,     0,     0,   329,   339,
       0,   177,     0,     0,     0,     0,     0,   338,     0,   340,
     341,   342,     0,     0,     0,   339,  2144,   177,   343,   734,
       0,     0,     0,     0,     0,   340,     0,     0,   330,     0,
       0,     0,     0,     0,     0,     0,   880,   881,   882,   883,
     884,   885,   886,   887,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   331,     0,     0,     0,  1322,  1323,
    1324,  1325,     0,     0,     0,   332,   333,   888,     0,   889,
     890,   891,   892,   893,   894,   895,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   334,     0,
       0,     0,     0,     0,   335,   336,   337,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   896,   897,     0,   338,     0,     0,     0,     0,
       0,     0,     0,   339,     0,   177,     0,     0,     0,     0,
       0,     0,     0,   340,   341,   342,     0,     0,     0,     0,
       0,     0,   343,   878,     0,     0,     0,   315,     0,     0,
       0,     0,     0,     0,     0,     0,   879,     0,     0,     0,
       0,     0,   316,   880,   881,   882,   883,   884,   885,   886,
     887,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   317,     0,     0,     0,   318,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1326,     0,     0,     0,
       0,     0,     0,     0,   888,   898,   889,   890,   891,   892,
     893,   894,   895,     0,     0,     0,     0,     0,     0,     0,
       0,   899,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   900,     0,     0,     0,     0,  1327,  1328,     0,
    1420, -1797,     0, -1797,     0,     0, -1797,     0,     0,   896,
     897,     0,     0,  1474,     0,     0,     0,     0,   901,   319,
       0, -1797,     0,     0,  1329,     0,     0,     0,     0,     0,
     320,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     902,     0,     0,     0,     0, -1708,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   903,     0,     0,     0,     0,
     321,   904,     0,   905,     0,     0,  1330,   322, -1734, -1797,
       0,  1475,     0,   324,     0,     0,   325,   906,   326,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0, -1734,
       0,   327,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   898,     0,     0,     0, -1797,     0,     0,   328,
       0,     0,     0,     0,     0,   188,     0,     0,   899,     0,
       0,     0,     0,     0, -1778,     0,     0,     0,     0,   900,
       0,     0,     0,     0,     0,   219,     0,   909,   910,   911,
       0,     0,     0,     0,  1331,     0, -1797,     0,     0,     0,
       0,   329,     0,     0,     0,   901,     0,     0,     0,     0,
       0,  1332,     0,     0,     0,     0, -1797,     0,     0,     0,
   -1797,     0,     0,     0,     0,     0,     0,   902,     0,     0,
       0,   330,     0, -1797,     0,     0,     0,   315,     0,  1333,
       0,     0,   903,     0,     0,   913,   914,   915,   904,     0,
     905,   315,   316,     0,     0,     0,     0,     0,   916,     0,
       0,   917,  1334,     0,   906,     0,   316,   331,     0,     0,
       0,   317, -1708,     0,     0,   318,     0,   907,   332,   333,
   -1797, -1797,  1335,     0,   908,   317,     0,     0,     0,   318,
   -1797,     0,     0,     0,     0,     0,     0,     0,     0,     0,
   -1797,   334,     0,     0,     0,     0,     0,   335,   336,   337,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
   -1174,     0,   219,     0,   909,   910,   911,     0,   338, -1797,
       0,     0,     0, -1174,     0,   315,   339,   123,  1476, -1708,
       0,     0,     0,  1474,     0,     0,   340,     0,     0,   319,
     316,   912,     0, -1797,     0,     0,     0,     0,     0,     0,
     320,     0,     0,   319,     0,     0,     0,     0,     0,   317,
   -1778,     0,     0,   318,   320,     0,     0, -1734,     0, -1797,
       0,     0,   913,   914,   915,     0, -1797,     0,     0,     0,
     321,     0,     0,     0,     0,   916, -1797,   322,   917,     0,
   -1797,   323,     0,   324,   321,     0,   325,     0,   326,     0,
       0,   322,     0,     0,     0,   323,     0,   324,     0,     0,
     325,   327,   326,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   327,     0,     0,     0,   328,
   -1797,     0,     0,     0,     0,     0,     0,   319,     0,  1588,
       0,     0,     0,   328,     0,     0,     0,     0,   320,     0,
       0,     0,     0,     0,     0,     0, -1797,     0,     0,     0,
       0,     0,     0,     0,     0,     0, -1797,     0,     0,     0,
       0,   329, -1797,     0,     0,     0,     0,     0,     0,     0,
       0,   123,     0,     0,     0,   329,     0,     0,     0,   323,
       0,   324,     0,     0,   325,     0,   326,     0,     0,     0,
       0,   330,     0,     0,     0,     0,     0,     0,     0,   327,
       0,     0,     0,     0,     0,   330,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   328,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   331,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   332,   333,
       0,   331,     0,     0,     0,     0,     0,     0,     0,     0,
   -1797,     0,   332,   333,     0,     0,     0,     0,     0,   329,
       0,   334,     0,     0,     0,     0,     0,   335,   336,   337,
       0,     0,     0,     0,     0,   334,     0,     0,     0,     0,
   -1174,   335,   336,   337,     0,     0,     0,     0,   338,   330,
       0,     0,     0, -1174,     0,     0,   339,   123,   177,     0,
       0,     0,   338,     0,     0,     0,   340,     0,     0,     0,
     339,     0,   177,     0,     0,     0,     0,     0,     0,     0,
     340,     0,     0,  -698,     0,   331,     0,  -698,  -698,  -698,
    -698,  -698,  -698,  -698,  -698,  -698,   332,   333,  -698,  -698,
    -698,     0,  -698,  -698,  -698,  -698,  -698,  -698,  -698,  -698,
    -698,  -698,     0,     0,     0,     0,     0,     0,     0,   334,
       0,     0,     0,     0,     0,   335,   336,   337,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   338,     0,     0,     0,
       0,     0,     0,     0,   339,     0,   177,     0,     0,     0,
       0,     0,     0,     0,   340,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -698,  -698,  -698,
    -698,  -698,  -698,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -698,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -698
};

static const yytype_int16 yycheck[] =
{
      59,    93,   204,   182,  1301,   266,   189,   186,   215,   370,
     217,   322,  1264,   939,   221,   436,  1235,  1623,  1047,  1371,
     798,  1620,  1241,  1623,  1420,  1277,  1623,  1551,  1156,  1003,
    1004,  1012,  1271,   722,   230,   368,   417,   874,  1285,   132,
     133,   134,  1369,   821,   137,   971,  1406,   140,     7,    10,
     945,     7,    10,   260,  1301,   718,   719,  1050,  1385,     7,
       5,    23,   241,     7,   271,    22,    18,    18,     7,   995,
     277,  1709,   255,   252,    10,     7,    77,    10,  1531,     7,
     917,    10,    54,   176,     8,   133,   265,    67,    64,    38,
    1750,    40,    41,    10,   187,    80,   122,   105,    56,  1757,
    1496,    95,   134,   133,   197,    42,    59,   134,   243,    22,
     372,   124,    24,   115,   475,  1269,   168,  1447,   211,   212,
      67,   214,  1819,  1074,  1421,   218,   192,   128,   129,  1599,
    1460,     6,   225,   220,   135,  1086,   243,  1590,    38,   520,
      40,    41,   202,     3,  1390,    90,  2265,   243,  1074,   187,
      67,    38,   788,    40,    41,    65,   248,    80,  1312,   971,
    1086,  1513,    55,   319,    80,   256,    29,   207,   126,   807,
     208,   834,  2333,   356,    29,    80,   269,  1507,    80,  2039,
     260,   270,   203,  1251,   121,    10,   134,    38,   371,    40,
      41,   299,  1810,   126,   182,   400,   215,    80,   217,   345,
    1527,  1096,   221,    96,  1501,    55,    55,    73,  1505,   220,
     297,   256,    82,   169,   200,  1299,    55,   213,   208,   280,
       0,     9,   314,   406,    97,   226,   396,   246,  1752,  1066,
     256,   124,  1316,   192,   488,   280,  1706,   220,    71,   135,
     234,   260,    80,   101,   234,   241,   380,  1865,    36,   341,
     342,   343,   271,  1407,  1408,  1409,  1503,   215,   277,   128,
     123,   353,    22,   324,  1293,    98,    99,    67,   123,   105,
     256,  1741,   135,   318,   367,   124,  1257,   194,   330,   971,
     450,   282,   215,   284,   489,   124,   230,   245,   233,   435,
    1587,   371,   237,  2454,   305,   256,   404,   242,   256,   105,
     393,   126,   239,   362,   439,   263,   128,   396,   946,   271,
     331,   184,   268,   400,   297,  1975,   472,   187,  1244,    31,
    1246,   309,   234,   256,   364,  1795,   243,   510,  1988,   366,
     263,  1324,   439,   969,   191,   348,  1974,   429,   430,  1419,
     432,   433,    54,   439,   971,  1591,   438,   439,   440,   441,
     442,   443,   444,   445,   446,   447,   448,   232,  1332,   216,
    1599,  1287,  1722,   273,   456,   457,   458,   459,   460,   461,
     462,   463,   464,   465,   581,   582,  1530,   466,   336,   128,
     473,   588,   473,    55,  2244,   971,   479,   256,   481,   482,
     215,  1721,   485,   486,  1754,  1848,   256,   490,   428,   384,
    1796,   234,   489,   336,  1857,  1657,   256,   473,   446,  1736,
    1490,    38,   374,    40,    41,  1366,  1228,  1391,   293,  1393,
     509,   473,  2080,  1930,   456,    10,  2123,    73,   488,   456,
    1900,   256,   268,  2552,   256,    96,   529,   300,   263,   646,
    1366,   133,   490,  1773,   402,  1805,   192,   473,  1800,  1478,
    1957,   388,   124,    80,   467,   348,  1708,   410,   665,  1097,
    1856,   384,   268,   124,   388,   473,   438,  1706,   384,   402,
     472,   473,  1446,   432,   468,   345,   569,   428,   436,   384,
     313,   688,   384,   195,   432,   432,   907,  1382,  1383,   325,
     466,  1738,  2152,  2153,   474,   102,  2156,   355,  2158,   348,
     432,   384,  1741,   436,   432,  1573,   491,   256,   456,   348,
    2017,   336,   473,   472,   473,   364,   473,   473,   470,   325,
     433,   387,   439,   481,   437,   473,   488,   620,  2188,   473,
     623,   624,   715,   626,   473,  2173,  1228,   473,   487,    23,
     723,   473,   254,   343,   473,   473,   384,   307,   481,    23,
     642,   643,    38,  2250,    40,    41,  1795,   764,   650,   578,
    2067,   473,   581,   582,   422,   435,  1255,  1256,   491,   588,
      38,  1901,    40,    41,   467,   491,   472,   402,   959,   524,
     581,   600,   482,   483,   484,   485,   491,   487,   625,   491,
      38,   798,    40,    41,   494,   482,   483,   484,   485,   692,
     487,  1228,  1954,  1955,   473,  2001,   133,   494,   491,   472,
     227,   436,    80,  2009,   821,   822,   473,   971,   467,   265,
    2127,    12,  2282,   473,   219,   718,   719,   646,   467,   995,
     231,   481,   725,   484,   485,  2207,   487,   473,  1019,   488,
     190,   848,  1228,   826,  1863,   369,   665,   207,   473,  2045,
     669,   473,   859,   491,  2107,   488,   481,    38,  1985,    40,
      41,  1900,  1747,   846,   665,  1328,    93,   473,   851,   688,
     763,  1092,  1093,   433,  2334,   365,   348,   437,    69,    55,
    1760,  1978,   865,   945,   330,   778,  2016,   348,  1583,  1682,
    1683,  1684,   364,   220,   787,  1384,   281,  2167,   189,  2169,
     234,   794,   207,   722,   396,   239,   190,  1848,   136,  2369,
    2370,  1548,   142,  2237,   105,   106,  1857,   108,  2378,   902,
      76,   814,   815,  1056,   473,   116,   438,   473,   431,   455,
    1949,    42,  1761,  1952,   827,   260,   382,   344,    24,   832,
     833,   387,   468,   207,   228,   764,  2216,   354,   619,   488,
     843,   433,   236,   624,   339,   437,   281,   384,   450,  2301,
     472,   256,   236,  2423,   206,    24,  2118,  2119,  2015,  1923,
     297,  2131,  2432,  2345,  2281,   776,   256,   869,   256,   798,
     869,   139,  2252,   473,  1765,   968,   108,   271,  1769,   218,
     181,   256,   495,   256,     9,   467,   154,   271,   186,  2129,
    1928,  2197,   821,   822,  1000,  1001,   467,   256,   900,   238,
     380,  2318,   372,  2320,    31,    33,   488,    38,  2170,    40,
      41,    36,   347,   916,   256,  2485,    32,   473,    82,   848,
    1915,   260,   126,   383,  1017,   196,  2134,   856,   857,   227,
     859,    38,   126,    40,    41,  2505,  2506,   235,  2355,   466,
      22,   575,   281,   283,   230,   482,   483,   484,   485,    80,
     487,  2257,   110,   248,   491,  2525,   257,   372,   295,   191,
     477,  1054,   473,   315,  1228,    98,  2482,   472,  1244,   971,
    2387,  2388,  2482,    55,  2214,  2482,  2458,   378,  2460,   908,
     374,  2433,    38,  2112,    40,    41,  2115,   253,  1081,   383,
     374,   292,   125,   995,   431,   260,   133,  2567,   372,   337,
     132,   222,   118,  1002,   175,   176,   384,  1946,  1947,  1948,
     308,   215,  2174,   448,    96,   142,   281,  2081,  2167,   314,
    2169,   215,   359,   187,   271,    38,  2263,    40,    41,   473,
    1032,   189,   253,   434,   205,  1038,  2165,   472,   234,  2301,
     211,  1044,   124,  1046,    28,  1047,   341,   342,   343,  2558,
     388,  2560,   256,   490,   320,   183,  2107,  1060,   353,   263,
    1062,  2323,   256,  1065,   401,   234,   403,  2216,   473,   263,
     234,   242,  1074,  2235,   182,   376,   247,  2322,   207,  2143,
    2144,   352,  2290,   473,  1086,   473,   482,   483,   484,   485,
     391,   487,   289,     0,   341,   491,   343,   255,   473,   207,
     473,   259,  1274,  2252,   482,   483,   484,   485,  1379,   487,
     434,   207,   265,   491,   473,  2532,  2533,    12,   419,   256,
     341,  1293,   481,  2187,   482,   483,   484,   485,   349,   487,
     469,   473,   336,  1226,   429,   430,   264,   432,   433,   481,
     256,   436,   336,   438,   439,   440,   441,   442,   443,   444,
     445,   446,   447,   448,  1083,  1843,  1125,    38,  2592,    40,
      41,   456,   457,   458,   459,   460,   461,   462,   463,   464,
     465,  2433,   473,   707,    69,   473,    38,   330,    40,    41,
     313,   345,   392,   488,  1273,   127,    38,   488,    40,    41,
    2435,   482,   483,   484,   485,   332,   487,   268,   402,   265,
     491,   309,  2409,   126,  2268,  1294,   299,   472,   402,  1298,
     105,   106,   308,   108,  1303,   299,   392,   299,  1390,   244,
     378,   116,  1311,   356,   256,   488,   392,  1229,  1231,   382,
    1232,   218,   436,  1235,  1237,    23,   268,  1240,    10,  1241,
     450,    13,  1244,   372,    16,    17,   153,   267,  1955,   488,
     157,   238,  1424,   384,   153,  1476,   364,   791,   157,  1352,
    1262,   458,  1483,  1594,   330,  1596,   348,  1269,   344,   230,
    1269,   435,   267,   260,   450,   472,   434,   481,  1280,  1281,
    2409,   313,     5,  1285,   450,  1287,   181,   481,  1460,  2534,
    2535,  1293,   215,  2442,   281,  2444,   203,    20,   318,  1301,
     218,   219,    29,   396,   203,    93,  1478,    30,  1310,   142,
    1312,   253,   396,  1312,   396,  1318,   382,   475,   489,    42,
     238,   492,   493,   318,  1413,   488,  1255,  1256,  1331,  1418,
    1333,   319,  1334,   256,   373,   488,  2581,  1509,   488,   265,
     263,   875,   260,   304,    75,   370,    77,   642,   643,   115,
      73,   482,   483,   484,   485,   650,   487,   450,   466,  1288,
     491,   432,   257,   281,  1366,   392,   450,  1296,   450,   432,
    1299,  1300,   411,  1302,   256,   482,   483,   484,   485,   300,
     487,   169,   380,   325,   491,   467,   341,  1316,  1719,    13,
    1392,   473,    16,    17,   233,   122,   123,   292,  1286,   281,
     233,  1266,   190,  1268,   330,  1407,  1408,  1409,  1407,  1408,
    1409,  2118,  2119,   336,   707,  1491,   323,  2253,  1420,  1591,
     327,  1309,   488,   450,   331,  1646,   482,   483,   484,   485,
     196,   487,   331,   195,   488,   491,  1553,   488,  2274,  1532,
     228,   473,   208,   398,   399,  1447,   392,   286,   236,   481,
     283,   213,  1528,   286,   173,  1384,   382,  1574,  1460,  1461,
     290,   291,   482,   483,   540,  1558,  1559,  1584,   544,   482,
     483,   484,   485,   549,   487,   473,  1478,    10,   491,   402,
    1652,   376,   469,   271,   472,   473,   488,   482,   483,   408,
     488,   410,    53,   423,  1496,   222,   391,   186,   791,   222,
     202,  1503,   274,   275,   450,  1507,   295,   295,  2344,   136,
    2346,    72,   339,   472,   233,  1517,    62,   234,  1520,    25,
     292,   293,   253,   254,   419,   195,   303,  1529,  1530,   488,
     253,  1530,   178,   473,  1537,  1538,   182,   260,   227,   256,
    1543,  2425,   473,   213,  1716,  2578,   235,  1550,   488,   110,
    1552,   469,    98,   472,   472,  2588,    26,    27,   481,    92,
     187,  1892,   488,  1492,  1566,   284,  1431,   286,   114,  1571,
     359,   359,   256,  2606,   107,  1578,   195,   282,   473,   125,
     249,   250,  2396,   306,  2398,   139,   374,   187,   114,   312,
      60,    61,  1764,   126,   213,   383,  2410,   130,   303,   125,
     154,   482,   483,   484,   485,   473,   487,    77,   208,    79,
     491,  1580,   401,   401,   403,   403,   260,   473,   341,   308,
     482,   483,   484,   485,  1553,   487,   349,    95,   260,   491,
     482,   483,   484,   485,  1806,   487,  1808,   281,   361,   491,
     260,   111,   112,   113,  1613,  1574,   314,   396,   316,   281,
     274,   275,   327,   433,   694,  1584,   234,   437,   698,   473,
     335,   281,   330,   488,   387,   388,   196,   390,   292,   293,
     298,   204,   300,   234,   341,   398,   343,   432,   433,   240,
     115,  2495,   215,   409,  1687,   440,   126,   234,   443,   444,
     251,  1694,   459,   460,   461,   462,   290,   291,   138,   169,
     341,   171,   343,  1678,  1679,  1680,  1681,    67,   172,   179,
     174,   434,   380,   391,   418,   419,   420,  2048,   391,  1721,
    1813,  1814,  1815,   256,  1817,   258,  1843,    75,   261,    77,
     263,   341,   285,   343,   287,   327,  1738,   175,   176,   172,
     172,   174,   174,   276,   726,   132,   728,  2344,   356,  2346,
     380,  1753,  1047,   290,   291,   253,   254,    14,    15,  1761,
     256,   294,  2160,  2161,   380,   488,  1768,   205,   380,    34,
     387,  1773,    37,   211,   380,   215,   231,   488,   269,   248,
     341,   342,    78,   234,   254,    50,    51,    52,  1790,   386,
     132,   488,   488,   488,  1796,   356,   380,   358,   268,   375,
    1802,   320,   490,   336,   242,   133,   256,  1979,    66,   247,
     268,   488,   380,   330,   641,   297,   256,   644,  1747,   490,
     647,   648,   649,   263,   490,   652,   653,   654,   490,   656,
     657,   658,   659,   366,   490,   100,   490,   277,   490,   490,
     278,  2012,   490,   490,   269,   314,   495,  2019,   490,   341,
     490,   490,   490,   490,  1856,   490,   388,   472,   409,   428,
     323,  1863,   488,   239,   473,  1868,   388,   364,   473,   402,
     491,  1874,   341,   342,   343,   388,   338,   491,   491,    42,
     413,   414,   405,   473,   353,   307,   301,   491,   409,   488,
     488,   256,  1894,  2065,   473,   488,   336,   488,    24,  1901,
    2072,   297,     8,   436,   256,   472,   467,    94,  2020,   442,
     443,   444,   177,    94,  1843,   488,   473,   135,   468,    18,
     396,  1923,   303,   117,  1923,   432,   220,   488,   254,   473,
     463,   300,   197,   473,   208,    28,   488,  1939,   471,   208,
     473,   193,  2213,   396,  1946,  1947,  1948,  1949,   481,   214,
    1952,   370,   245,   481,   470,   432,    28,    31,   195,   429,
     429,   430,   402,   432,   433,   405,   406,   436,   488,   438,
     439,   440,   441,   442,   443,   444,   445,   446,   447,   448,
     380,   290,    37,   428,  2101,   431,   256,   456,   457,   458,
     459,   460,   461,   462,   463,   464,   465,   262,  1293,  2001,
     117,   256,   385,   473,   434,   326,   440,  2009,  2010,   440,
     122,   256,   473,  2015,  2016,   488,   281,   488,   115,   223,
    1989,   212,   470,   417,   289,    55,   389,   143,   225,  1998,
     144,    67,   372,   146,   149,  2037,   109,   427,    55,   150,
     432,   481,   428,  2045,   200,   310,   155,   431,   425,   200,
     468,   489,   357,   161,   492,   493,   120,   164,   404,   182,
     213,   200,   313,   473,  2066,     9,   432,   207,   426,  2071,
    2072,   432,   364,   148,   285,    37,    10,   466,   432,  2081,
     151,   184,  2081,   390,   256,   470,   230,   192,   470,   432,
      55,   260,   230,   473,   194,   433,   200,   167,   200,  2102,
      37,   480,   367,   239,   479,    66,   473,   329,   364,   255,
    2112,   408,   377,  2115,  2117,   466,   397,   207,   286,    18,
     364,   355,   428,   289,    55,  1420,     9,  2129,   156,  2218,
    2099,   200,   142,   470,   470,   260,   260,   432,   239,   260,
      10,  2143,  2144,   327,  2143,  2144,   328,   141,   195,    55,
     432,   288,   352,   207,   368,    78,   152,   154,   118,   467,
     432,    55,   196,  2165,   467,   260,   159,   309,   160,   260,
     305,   319,  2101,   642,   643,   121,   422,     8,   104,   256,
     248,   650,   447,  1478,   351,  2187,  2358,   283,  2187,   351,
     154,   154,   110,    55,   260,  2197,  2368,   260,   260,   282,
     287,  1496,   104,  2205,   162,   284,  2209,   163,   147,   166,
     145,    55,  2214,   274,   435,   239,   165,   327,   368,  2221,
     455,  1263,   120,   304,   304,   158,   918,   175,   175,   327,
      56,    31,  1274,  1275,    33,  2352,   126,    78,   327,   306,
    2243,   105,  2171,   116,  1774,   142,   314,  1289,  1290,   824,
     281,  2253,   505,   592,    39,  2257,   673,    41,   572,   953,
     480,   665,   396,  1305,  1306,  2217,  2268,  2270,  2271,  2268,
      44,    47,   252,   341,   342,   343,   168,  1016,   613,   994,
     857,  1228,  1373,  1378,  2014,   353,  1841,  1228,  2400,  2273,
     610,   871,  1321,  2602,  2086,  1347,  2298,  1559,   791,   599,
     958,  1596,  2479,  2305,  2220,  1030,  1591,  1650,  2138,  2433,
    2139,  2302,  2269,  2141,  1935,  1424,  2323,  1708,  1711,  1974,
    2173,  1444,  2415,  2416,  2338,  1979,  1726,  1731,  2205,  2007,
    2022,  1478,  2221,  2225,  2368,  1806,  1769,  2239,  1503,  1501,
    1302,  1787,  2344,  2072,  2346,  2394,  1514,  2072,  1588,  2399,
    2552,  2463,  2230,  2405,  1396,  1639,  1937,  1492,  1755,  2048,
    1571,   429,   430,  1405,   432,   433,   527,  1306,   456,   249,
     438,   439,   440,   441,   442,   443,   444,   445,   446,   447,
     448,  1364,  1424,   751,  2386,  1604,   717,  1736,   456,   457,
     458,   459,   460,   461,   462,   463,   464,   465,  2512,   585,
     869,   310,  1444,   488,  1715,   136,  1838,  2409,  1239,    -1,
      -1,    -1,    -1,  1455,    -1,  2504,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2352,  2426,   517,  1468,    -1,    -1,    -1,
      -1,    -1,  2544,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      42,    43,    44,    45,    46,    47,    48,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2543,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1761,    -1,  1510,    -1,
      -1,    -1,  1514,    -1,    -1,    -1,    -1,  1519,    -1,    -1,
      -1,    83,    -1,    85,    86,    87,    88,    89,    90,    91,
      -1,    -1,  2494,    -1,    -1,  2497,    -1,    -1,    -1,    -1,
    2589,  1796,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2513,    -1,    -1,  2517,    -1,    -1,  2446,    -1,  2521,
      -1,    -1,    -1,    -1,    -1,    -1,   128,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2466,    -1,  2542,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2551,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1599,    -1,    -1,
      -1,  1856,    -1,    -1,    -1,    -1,    -1,  2569,    -1,    -1,
      -1,    -1,    -1,    -1,   642,   643,    -1,    -1,  1047,    -1,
      -1,    -1,   650,    -1,    -1,    -1,    -1,  2590,    -1,    -1,
      -1,  2593,    -1,    -1,    -1,    -1,    -1,  1639,    -1,    -1,
      -1,     1,    -1,    -1,    -1,    -1,    -1,     7,    -1,    -1,
      10,    -1,    -1,    13,    14,    15,    -1,    -1,    -1,   221,
      -1,    21,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    38,    39,
      40,    41,    -1,    -1,    -1,    -1,  2615,  2616,    -1,    -1,
      -1,  1946,  1947,  1948,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1705,  1706,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   274,    -1,  1716,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    92,    -1,    -1,    -1,    -1,    -1,    -1,  1741,
      -1,    -1,    -1,    -1,    -1,    -1,  2001,   107,    -1,   311,
      -1,    -1,    -1,    -1,  2009,  2010,    -1,   319,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   126,    -1,    -1,    -1,
     130,   333,    -1,    -1,    -1,  1777,    -1,    -1,    -1,    -1,
      -1,  1783,  1784,    -1,    -1,  1787,    -1,  1789,    -1,    -1,
    2045,    -1,    -1,  1795,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   175,   176,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1827,    -1,    -1,   188,    -1,
      -1,   393,   394,   395,    -1,    -1,    -1,    -1,    -1,    -1,
    1269,    -1,    -1,    -1,   204,   205,    -1,    -1,    -1,    -1,
      -1,   211,    -1,    -1,    -1,   215,    -1,    -1,  1287,    -1,
      -1,    -1,    -1,    -1,  1293,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   234,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   242,  1312,    -1,   245,    -1,   247,    -1,   451,
     452,   453,   252,    -1,    -1,    -1,   256,    -1,   258,    -1,
      -1,   261,    -1,   263,    -1,    -1,    -1,     7,    -1,    -1,
      10,   473,    -1,    -1,    -1,    -1,   276,    -1,   278,   279,
      -1,    -1,   282,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   292,    -1,   294,    -1,    -1,    -1,    -1,   299,
      -1,    -1,  2197,    -1,   304,    -1,    -1,    -1,    -1,    -1,
    2205,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   321,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1977,   336,    -1,  1407,  1408,
    1409,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     0,
       1,  1420,    92,     4,    -1,     6,    -1,    -1,    -1,    -1,
      11,    -1,  2257,    -1,    -1,    -1,   366,   107,    19,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1047,
      -1,    -1,    -1,    -1,    -1,    -1,   126,    -1,    -1,    -1,
     130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   402,    -1,    -1,    -1,    57,    58,    -1,  1478,
    2052,    -1,    63,   413,   414,    -1,   416,    68,    -1,    70,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1496,    -1,    -1,
      81,    -1,    -1,    84,    -1,   175,   436,    -1,    -1,    -1,
      -1,    -1,   442,   443,   444,    -1,    97,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1530,    -1,   463,   204,    -1,    -1,    -1,   119,    -1,
      -1,   471,    -1,   473,    -1,   215,    -1,   128,    -1,    -1,
     131,   481,   482,   483,   484,   485,    -1,   487,    -1,   489,
     490,   491,   492,   493,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   153,    -1,    -1,   245,   157,    -1,    -1,    -1,
      -1,    -1,   252,    -1,    -1,    -1,   256,   168,   258,    -1,
      -1,   261,    -1,   263,    -1,    -1,    -1,  1596,    -1,   180,
      -1,    -1,    -1,   184,   185,    -1,   276,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   199,    -1,
      -1,    -1,   203,    -1,   294,   206,    -1,    -1,   209,   210,
      -1,    -1,    -1,    -1,    -1,    -1,   217,    -1,    -1,    -1,
      -1,    -1,    -1,   224,    -1,   226,    -1,    -1,   229,    -1,
    2222,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   336,    -1,    -1,    -1,
      -1,    -1,    31,    32,    -1,    34,    -1,    -1,    37,    -1,
      -1,    -1,    -1,    -1,    -1,   266,    -1,    -1,    -1,   270,
      -1,   272,    -1,    52,    -1,  1293,   366,    -1,    -1,   280,
    2272,    -1,    -1,   139,    -1,    -1,    -1,   143,   144,   145,
     146,   147,   148,   149,   150,   151,    -1,    76,   154,   155,
     156,   302,   158,   159,   160,   161,   162,   163,   164,   165,
     166,   167,   402,    -1,    -1,   316,    -1,    -1,    -1,    -1,
      -1,   100,   323,   413,   414,    -1,    -1,    -1,    -1,    -1,
     331,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   339,   340,
      -1,    -1,  1761,    -1,    -1,    -1,   436,    -1,    -1,   350,
      -1,    -1,   442,   443,   444,    -1,    -1,    -1,   137,    -1,
      -1,   362,   363,    -1,    -1,    -1,    -1,    -1,   369,    -1,
     371,    -1,    -1,   463,    -1,    -1,    -1,  1796,   379,    -1,
      -1,   471,    -1,   473,    -1,    -1,    -1,    -1,   389,    -1,
      -1,   481,    -1,    -1,    -1,    -1,    -1,   398,   177,   489,
      -1,    -1,    -1,    -1,    -1,    -1,   407,    -1,    -1,    -1,
     411,   412,    -1,    -1,   415,    -1,    -1,   418,   197,    -1,
      -1,   200,   201,   424,    -1,    -1,    -1,   283,   284,   285,
     286,   287,   288,    -1,    -1,   214,     1,  1856,    -1,     4,
     441,     6,    -1,    -1,    -1,    -1,    11,    -1,   449,    -1,
      -1,    -1,    -1,   454,    19,    -1,    -1,    -1,    -1,    -1,
    1478,    -1,    -1,    -1,   465,    -1,    -1,    -1,    32,    -1,
      34,    -1,   473,    37,   253,   476,    -1,   478,  2470,    -1,
      -1,    -1,   261,   262,    -1,    -1,    -1,   488,    52,  2481,
      -1,    -1,    57,    58,    -1,    -1,    -1,    -1,    63,    -1,
      -1,    -1,   281,    68,  1923,    70,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    81,    -1,    -1,    84,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1946,  1947,  1948,
      -1,   310,    97,    -1,    -1,    -1,   100,  2529,    -1,    -1,
      -1,   320,    -1,    -1,    -1,  2537,    -1,    -1,    -1,  2541,
      -1,    -1,    -1,    -1,   119,   334,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   128,    -1,    -1,   131,    -1,    -1,    -1,
      -1,    -1,    -1,   137,    -1,    -1,    -1,   142,    -1,    -1,
      -1,   360,  2001,    -1,    -1,    -1,    -1,    -1,   367,    -1,
    2009,  2010,    -1,    -1,    -1,    -1,    -1,    -1,   377,    -1,
      -1,    -1,   381,   168,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   177,   470,   180,    -1,    -1,    -1,   184,
     185,    -1,    -1,    -1,    -1,    -1,  2045,    -1,    -1,    -1,
      -1,    -1,   488,   197,   199,    -1,    -1,   201,    -1,    -1,
      -1,   206,   421,    -1,   209,   210,    -1,    -1,    -1,    -1,
     214,   430,   217,    -1,    -1,    -1,    -1,    -1,    -1,   224,
      -1,   226,  2081,    -1,   229,    -1,    -1,    -1,   447,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   457,    -1,
      -1,    -1,    -1,    -1,   463,    -1,    -1,    -1,     7,    -1,
      -1,    10,    -1,   472,    -1,    -1,    -1,   261,   262,    -1,
      -1,   266,    -1,    -1,    -1,   270,    -1,   272,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   280,    -1,   281,    -1,    -1,
      -1,    -1,    -1,    -1,  2143,  2144,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    55,   302,    -1,    -1,
      -1,    -1,    -1,  1761,    -1,    -1,   310,    -1,    -1,    -1,
      -1,   316,    -1,    -1,    -1,    -1,    -1,    -1,   323,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2187,    -1,
     334,    -1,    -1,    92,   339,   340,    -1,    96,  2197,    -1,
      -1,    -1,    -1,    -1,    -1,   350,  2205,     7,   107,    -1,
      10,    -1,    -1,    13,    14,    15,   360,   362,   363,    -1,
      -1,    21,    -1,   367,   369,   124,   371,   126,    -1,    -1,
      -1,   130,    -1,   377,   379,    -1,    -1,   381,    38,    39,
      40,    41,    -1,    -1,   389,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   398,  2253,    -1,    -1,    -1,  2257,    -1,
      -1,    -1,   407,    -1,    -1,    -1,   411,   412,    -1,  2268,
     415,    -1,    -1,   418,    -1,    -1,    -1,   421,    -1,   424,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    92,    -1,    -1,    -1,   441,    -1,    -1,    -1,
      -1,    -1,    -1,   447,   449,   204,    -1,   107,    -1,   454,
      -1,    -1,    -1,   457,    -1,    -1,   215,    -1,    -1,   463,
     465,    -1,    -1,    -1,    -1,    -1,   126,    -1,   473,    -1,
     130,   476,    -1,   478,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   488,    -1,  2344,   245,  2346,  1946,  1947,
    1948,    -1,    -1,   252,    -1,    -1,    -1,   256,    -1,   258,
      -1,    -1,   261,    -1,   263,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   175,   176,   276,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   188,    -1,
      -1,    -1,    -1,    -1,    -1,   294,    -1,    -1,    -1,    -1,
     299,    -1,    -1,    -1,   204,   205,    -1,    -1,    -1,    -1,
      -1,   211,    -1,    -1,    10,   215,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   234,    -1,    -1,   336,    -1,    -1,
      -1,    -1,   242,    39,    -1,   245,    -1,   247,    -1,   348,
      -1,    -1,   252,    -1,    -1,    -1,   256,    -1,   258,    -1,
      -1,   261,    -1,   263,    -1,    -1,    -1,   366,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   276,    -1,   278,   279,
      -1,    -1,   282,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   292,    -1,   294,    -1,    92,   396,    -1,   299,
      -1,    -1,    -1,   402,   304,    -1,    -1,    -1,    -1,    -1,
      -1,   107,    -1,    -1,   413,   414,    -1,    -1,    -1,    -1,
      -1,   321,    -1,    -1,     4,    -1,     6,    -1,    -1,    -1,
     126,    11,    -1,    -1,   130,    -1,   336,   436,    -1,    19,
      -1,    -1,    -1,   442,   443,   444,    -1,    -1,    -1,    -1,
      -1,   450,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   463,    -1,   366,    -1,   467,    -1,
      -1,    -1,   471,    -1,   473,    -1,    -1,    57,    58,    -1,
      -1,    -1,   481,    63,    -1,    -1,    -1,    -1,    68,    -1,
      70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    81,   402,    -1,    84,    -1,    -1,    -1,   204,    -1,
      -1,    -1,    -1,   413,   414,    -1,   416,    97,    -1,   215,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   436,    -1,    -1,   119,
      -1,    -1,   442,   443,   444,    -1,    -1,    -1,   128,   245,
      -1,   131,    -1,    -1,    -1,    -1,   252,    -1,    -1,    -1,
     256,    -1,   258,   463,    -1,   261,    -1,   263,    -1,    -1,
      -1,   471,    -1,   473,    -1,    -1,    -1,    -1,    -1,    -1,
     276,   481,   482,   483,   484,   485,    -1,   487,   168,   489,
     490,   491,   492,   493,    -1,    -1,    -1,    -1,   294,    -1,
     180,    -1,    -1,    -1,   184,   185,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   199,
      -1,    -1,    -1,    -1,    -1,    -1,   206,    -1,    -1,   209,
     210,    -1,    -1,    -1,    -1,    -1,    -1,   217,    -1,    -1,
     336,    -1,    -1,    -1,   224,    -1,   226,    -1,    -1,   229,
      -1,    -1,    -1,    -1,    -1,    -1,   139,    -1,    -1,    -1,
     143,   144,   145,   146,   147,   148,   149,   150,   151,    -1,
     366,   154,   155,   156,    -1,   158,   159,   160,   161,   162,
     163,   164,   165,   166,   167,    -1,   266,    -1,    -1,    -1,
     270,    -1,   272,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     280,    -1,    -1,    -1,    -1,    -1,   402,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   413,   414,    -1,
      -1,    -1,   302,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   316,    -1,    -1,    -1,
     436,    -1,    -1,   323,    -1,    -1,   442,   443,   444,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   339,
     340,    -1,    -1,    -1,    -1,    -1,    -1,   463,    -1,    -1,
     350,    -1,    -1,    -1,    -1,   471,    -1,   473,    -1,    -1,
      -1,    -1,   362,   363,    -1,   481,   482,   483,    -1,   369,
      -1,   371,    -1,    -1,   490,    -1,    -1,    -1,    -1,   379,
     283,   284,   285,   286,   287,   288,    -1,    -1,    -1,   389,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   398,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   407,    -1,    -1,
      -1,   411,   412,    -1,    -1,   415,    -1,    -1,   418,    -1,
      -1,   248,    -1,    -1,   424,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   441,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   449,
      -1,    -1,    -1,    -1,   454,    -1,    -1,    10,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   465,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   476,    -1,   478,    32,
      -1,    34,    -1,    -1,    37,    -1,    -1,   314,   488,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   341,   342,   343,    -1,    -1,    -1,
      -1,    -1,    -1,    76,    -1,    -1,   353,    -1,    -1,    -1,
      83,    -1,    85,    86,    87,    88,    89,    90,    91,    -1,
      -1,    -1,    -1,    -1,    -1,    98,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     7,    -1,    -1,
      10,    -1,    -1,    -1,    -1,    -1,    -1,   470,    -1,    -1,
      -1,    -1,    -1,   126,    -1,   128,   129,    -1,    -1,    -1,
      -1,    31,    -1,    -1,    -1,   488,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   430,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   438,   439,    -1,   441,   442,    -1,    -1,   445,   446,
     447,   448,    -1,    -1,   177,    -1,    76,    -1,    -1,   456,
     457,   458,   459,   460,   461,   462,   463,   464,   465,    -1,
      -1,    -1,    92,    -1,   197,    -1,    -1,   200,   201,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   107,    -1,    -1,
      -1,   214,   215,    -1,    -1,    -1,    -1,    -1,   221,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   126,    -1,    -1,    -1,
     130,    -1,    -1,    -1,   237,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   248,    -1,    -1,    -1,    -1,
     253,    -1,    -1,   256,    -1,    -1,    -1,    -1,   261,   262,
     263,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   274,    -1,    -1,    -1,    -1,    -1,    -1,   281,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   296,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   204,    -1,    -1,   310,   311,    -1,
      -1,    -1,    -1,    -1,   317,   215,   319,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     333,   334,    -1,   336,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     4,   245,     6,    -1,    -1,    -1,
      -1,    11,   252,   253,    -1,    -1,   256,   360,   258,    19,
      -1,   261,    -1,   263,   367,   642,   643,    -1,    -1,    -1,
      -1,    -1,    -1,   650,    -1,    -1,   276,    -1,   381,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   391,    -1,
     393,   394,   395,   396,   294,    -1,    -1,    57,    58,   402,
      -1,    -1,    -1,    63,    -1,    -1,    -1,    -1,    68,    -1,
      70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   421,    -1,
     320,    81,    -1,    -1,    84,    -1,    -1,    -1,    -1,   432,
      -1,    -1,    -1,   436,    -1,    -1,   336,    97,    -1,    -1,
      -1,    -1,    -1,    -1,   447,    -1,    -1,    -1,   451,   452,
     453,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   119,
     463,   464,    -1,   466,   467,    -1,   366,    -1,   128,    -1,
     473,   131,    -1,    -1,    -1,    -1,    -1,    -1,   481,    -1,
      -1,    -1,    -1,    -1,    -1,   488,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   402,    -1,    -1,    -1,    -1,    -1,   168,    -1,
      -1,    -1,    -1,   413,   414,    -1,    -1,    -1,    -1,    -1,
     180,    -1,    -1,    -1,   184,   185,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   436,    -1,    -1,   199,
      -1,    -1,   442,   443,   444,    -1,   206,    -1,    -1,   209,
     210,    -1,     7,    -1,    -1,    10,    -1,   217,   458,    -1,
      -1,    -1,    -1,   463,   224,    -1,   226,    -1,    -1,   229,
      -1,   471,   472,   473,    -1,    -1,    31,    -1,    -1,    34,
      -1,   481,    37,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    50,    51,    52,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   266,    -1,    -1,    -1,
     270,    -1,   272,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     280,    76,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    92,    -1,    -1,
      -1,    -1,   302,    -1,    -1,   100,    -1,    -1,     7,    -1,
      -1,    10,   107,    -1,    -1,    -1,   316,    -1,    -1,    -1,
      -1,    -1,    -1,   323,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   126,    -1,    -1,    -1,   130,    -1,    -1,    -1,   339,
     340,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     350,    -1,     7,    -1,    -1,    10,    -1,    -1,    -1,    -1,
      -1,    -1,   362,   363,    -1,    -1,    -1,    -1,    -1,   369,
      -1,   371,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   379,
      -1,    -1,   177,    -1,    -1,    -1,    -1,   182,    -1,   389,
      -1,    -1,    -1,    92,    -1,    -1,    -1,    -1,   398,    -1,
      -1,    -1,   197,    -1,    -1,    -1,    -1,   407,   107,   204,
      -1,   411,   412,    -1,    -1,   415,    -1,    -1,   418,   214,
     215,    -1,    -1,    -1,   424,    -1,    -1,   126,    -1,    -1,
      -1,   130,    -1,    -1,    -1,    -1,    -1,    92,    -1,    -1,
      -1,   441,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   449,
     245,    -1,   107,    -1,   454,    -1,    -1,   252,   253,    -1,
      -1,   256,    -1,   258,    -1,   465,   261,   262,   263,   168,
      -1,   126,    -1,    -1,    -1,   130,   476,    -1,   478,    -1,
      -1,   276,    -1,    -1,    -1,    -1,   281,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   289,    -1,    -1,    -1,    -1,   294,
      -1,    -1,    -1,    -1,    -1,   204,    -1,    -1,    -1,    -1,
      -1,     7,    -1,    -1,    10,   310,   215,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   320,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   336,    -1,    -1,    -1,    -1,   245,    -1,    -1,   204,
      -1,    -1,   207,   252,    -1,    -1,    -1,   256,    -1,   258,
     215,    -1,   261,    -1,   263,    -1,    -1,    -1,    -1,    65,
      -1,   366,   367,    -1,    -1,    -1,    -1,   276,    -1,    -1,
      -1,    -1,   377,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     245,    -1,    -1,    -1,    -1,   294,    92,   252,    -1,     7,
      -1,   256,    10,   258,    -1,    -1,   261,   402,   263,    -1,
      -1,   107,    -1,     7,    -1,    -1,    10,    -1,   413,   414,
      -1,   276,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     126,    -1,    -1,    -1,   130,    -1,    -1,   336,    -1,   294,
      -1,   436,    -1,    -1,    -1,    -1,    -1,   442,   443,   444,
      -1,    -1,   447,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   458,    -1,    -1,    -1,   366,   463,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   471,   472,   473,    -1,
      -1,   336,    -1,    -1,    92,    -1,   481,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    92,   107,
      -1,    -1,    -1,   402,    -1,    -1,    -1,    -1,   204,   364,
      -1,   366,    -1,   107,   413,   414,    -1,    -1,   126,   215,
      -1,    -1,   130,    -1,    -1,    -1,   120,    -1,    -1,    -1,
      -1,    -1,   126,    -1,   433,    -1,   130,   436,   437,    -1,
      -1,    -1,    -1,   442,   443,   444,    -1,   402,    -1,   245,
      -1,    -1,    -1,    -1,    -1,    -1,   252,    -1,   413,   414,
     256,    -1,   258,    -1,   463,   261,    -1,   263,    -1,    -1,
      -1,    -1,   471,    -1,   473,    -1,    -1,   273,    -1,    -1,
     276,   436,   481,    -1,    -1,    -1,    -1,   442,   443,   444,
      -1,    -1,    -1,    -1,    -1,    -1,   204,    -1,   294,   207,
      -1,    -1,     7,    -1,    -1,    10,    -1,   215,   463,    -1,
     204,    -1,    -1,    -1,    -1,    -1,   471,    -1,   473,    -1,
      -1,   215,    -1,    -1,    -1,    -1,   481,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   230,   245,    -1,    -1,
     336,    -1,    -1,    -1,   252,    -1,    -1,    -1,   256,    -1,
     258,   245,    -1,   261,    -1,   263,    -1,    -1,   252,    -1,
      -1,    -1,   256,    -1,   258,    -1,    -1,   261,   276,   263,
     366,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   276,    -1,    -1,    -1,   294,    92,     7,    -1,
      -1,    10,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     294,    -1,   107,    -1,    -1,    -1,   402,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   413,   414,    -1,
      -1,   126,    -1,    -1,    -1,   130,    -1,    -1,   336,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     436,    -1,   336,    -1,    -1,    -1,   442,   443,   444,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   364,    -1,   366,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   463,    -1,    -1,
      -1,    -1,   366,    92,    -1,   471,     7,   473,    -1,    10,
      -1,    -1,    -1,    -1,   103,   481,    -1,    -1,   107,    -1,
      -1,    -1,    -1,    -1,   402,    -1,    -1,    -1,    -1,   204,
      -1,    -1,    -1,    -1,    -1,   413,   414,   126,   402,    -1,
     215,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   413,
     414,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   436,    -1,
      -1,    -1,    -1,    -1,   442,   443,   444,    -1,    -1,    -1,
     245,    -1,   436,    -1,    -1,    -1,    -1,   252,   442,   443,
     444,   256,    -1,   258,    -1,   463,   261,    -1,   263,    -1,
      -1,    92,    -1,   471,     7,   473,    -1,    10,    -1,   463,
      -1,   276,   103,   481,    -1,    -1,   107,   471,    -1,   473,
      -1,    -1,    -1,    -1,    -1,   204,    -1,   481,    -1,   294,
      -1,    -1,    -1,    -1,    -1,   126,   215,    -1,    -1,   130,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   245,    -1,    -1,    -1,
      -1,   336,    -1,   252,    -1,    -1,    -1,   256,    -1,   258,
      -1,    -1,   261,    -1,   263,    -1,    -1,    -1,    -1,    92,
       7,    -1,    -1,    10,    -1,    -1,    -1,   276,    -1,    -1,
      -1,   366,    -1,    -1,   107,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   204,    -1,   294,    -1,   120,    -1,    -1,
      -1,    -1,    -1,   126,   215,    -1,    -1,   130,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   402,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   413,   414,
      -1,    -1,    -1,    -1,   245,    -1,    -1,   336,    -1,    -1,
      -1,   252,    -1,    -1,    -1,   256,    -1,   258,    -1,    -1,
     261,   436,   263,    -1,    -1,    92,    -1,   442,   443,   444,
      -1,    -1,    -1,    -1,    -1,   276,   103,   366,    -1,    -1,
     107,    -1,    -1,   458,    -1,    -1,    -1,    -1,   463,    -1,
      -1,   204,    -1,   294,    -1,    -1,   471,   472,   473,   126,
      -1,    -1,   215,   130,    -1,    -1,   481,    -1,    -1,    -1,
      -1,    -1,    -1,   402,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   413,   414,    -1,    -1,    -1,    -1,
      -1,    -1,   245,    -1,    -1,   336,    -1,    -1,    -1,   252,
      -1,    -1,    -1,   256,    -1,   258,    -1,   436,   261,    -1,
     263,    -1,    -1,   442,   443,   444,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   276,    -1,   366,    -1,    -1,    -1,     7,
      -1,    -1,    10,    -1,   463,    -1,    -1,   204,    -1,    -1,
      -1,   294,   471,     7,   473,    -1,    10,    -1,   215,    -1,
      -1,    -1,   481,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   402,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   413,   414,    -1,    -1,    -1,    -1,   245,    -1,
      -1,    -1,    -1,   336,    -1,   252,    -1,    -1,    -1,   256,
      -1,   258,    -1,    -1,   261,   436,   263,    -1,    -1,    -1,
      -1,   442,   443,   444,    -1,    -1,    -1,    -1,    -1,   276,
      -1,    -1,    -1,   366,    92,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   463,    -1,    -1,    -1,    -1,   294,    92,   107,
     471,     7,   473,    -1,    10,    -1,    -1,    -1,    -1,    -1,
     481,    -1,    -1,   107,    -1,    -1,    -1,    -1,   126,   402,
      -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     413,   414,   126,    -1,    -1,    -1,   130,    -1,    -1,   336,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   436,    -1,    -1,    -1,    -1,    -1,   442,
     443,   444,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   366,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     463,    -1,    -1,    -1,    -1,    -1,    92,    -1,   471,    -1,
     473,    -1,    -1,    -1,    -1,    -1,   204,    -1,   481,    -1,
      -1,   107,    -1,    -1,    -1,   402,   200,   215,    -1,    -1,
     204,    -1,    -1,    -1,    -1,    -1,   413,   414,    -1,    -1,
     126,   215,    -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   245,    -1,   436,
      -1,    -1,    -1,    -1,   252,   442,   443,   444,   256,    -1,
     258,   245,    -1,   261,    -1,   263,    -1,    -1,   252,    -1,
       7,    -1,   256,    10,   258,    -1,   463,   261,   276,   263,
      -1,    -1,    -1,    -1,   471,    -1,   473,     7,    -1,    -1,
      10,    -1,   276,    -1,   481,    -1,   294,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   204,    -1,
     294,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   215,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   336,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   245,
      -1,    -1,   336,    -1,    -1,    92,   252,    -1,    -1,    -1,
     256,    -1,   258,    -1,    -1,   261,    -1,   263,   366,    -1,
     107,    -1,    92,    -1,    10,    -1,    -1,    -1,    -1,    -1,
     276,    -1,   366,    -1,    -1,    -1,    -1,   107,    -1,   126,
      -1,    -1,    -1,   130,    -1,    -1,    -1,    -1,   294,    -1,
      -1,    -1,    -1,    -1,   402,    -1,   126,    -1,    -1,    -1,
     130,    -1,    -1,    -1,    -1,   413,   414,    -1,   402,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   413,
     414,    67,    -1,    -1,   432,    -1,    -1,    -1,   436,    -1,
     336,    -1,    -1,    -1,   442,   443,   444,    -1,    -1,    -1,
      -1,    -1,   436,    -1,    -1,    -1,    92,    -1,   442,   443,
     444,    -1,    -1,    -1,    -1,   463,    -1,   204,    -1,    -1,
     366,   107,    -1,   471,    -1,   473,    -1,    -1,   215,   463,
      -1,    -1,    -1,   481,   204,    -1,    -1,   471,    -1,   473,
     126,    -1,    -1,    -1,   130,   215,    -1,   481,    -1,    -1,
     396,    -1,    -1,    -1,    -1,    -1,   402,    -1,   245,    -1,
      -1,    -1,    -1,    -1,    10,   252,    -1,   413,   414,   256,
      -1,   258,    -1,    -1,   261,   245,   263,    -1,    -1,    -1,
      -1,    -1,   252,    -1,    -1,    -1,   256,    -1,   258,   276,
     436,   261,    -1,   263,    -1,    -1,   442,   443,   444,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   276,   294,   194,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   463,   204,    -1,
      -1,    67,    -1,    -1,   294,   471,    -1,   473,    -1,   215,
      -1,    -1,    -1,    -1,    -1,   481,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    92,    -1,    -1,   336,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   243,    -1,    -1,
      -1,   107,    -1,    -1,    -1,    -1,   336,    -1,    -1,    -1,
     256,    -1,   258,    -1,    -1,   261,    -1,   263,    -1,   366,
     126,    -1,    -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,
     276,    -1,    -1,    -1,    39,    -1,   366,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   294,    -1,
     397,    -1,    -1,    -1,    -1,   402,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   413,   414,    -1,    -1,
      -1,    -1,   402,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   413,   414,    -1,    -1,    92,    -1,   436,
     336,    -1,    -1,    -1,    -1,   442,   443,   444,   204,    -1,
      -1,    -1,   107,    -1,    -1,    -1,   436,    -1,    -1,   215,
      -1,    -1,   442,   443,   444,    -1,   463,    -1,    -1,    -1,
     366,   126,    -1,    -1,   471,   130,   473,    -1,    -1,    -1,
      -1,    -1,    -1,   463,   481,    -1,    -1,   243,    -1,    -1,
      -1,   471,    -1,   473,    -1,    -1,    -1,    39,    -1,    -1,
     256,   481,   258,    -1,    -1,   261,   402,   263,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   413,   414,    -1,
     276,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   294,    -1,
     436,    -1,    -1,   439,    -1,    -1,   442,   443,   444,   204,
      92,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     215,    -1,    -1,    -1,    -1,   107,    92,   463,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   471,    -1,   473,    -1,    -1,
     336,   107,    -1,    -1,   126,   481,    -1,    -1,   130,    -1,
     245,    -1,    -1,    -1,    -1,    -1,    -1,   252,    -1,    -1,
     126,   256,    -1,   258,   130,    -1,   261,    -1,   263,    -1,
     366,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   276,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   294,
      -1,    -1,    -1,    -1,    -1,    -1,   402,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   413,   414,    -1,
      -1,    -1,   204,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   215,    -1,    -1,    -1,    -1,   204,    -1,
     436,   336,    -1,   439,    92,    -1,   442,   443,   444,   215,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   107,
      -1,    -1,    -1,   245,    -1,    -1,    -1,   463,    -1,    -1,
     252,   366,    -1,    -1,   256,   471,   258,   473,   126,   261,
      -1,   263,   130,    -1,    -1,   481,    -1,    -1,    -1,    -1,
     256,    -1,   258,    -1,   276,   261,    -1,   263,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   402,    -1,    -1,
     276,    -1,   294,    -1,    -1,    -1,    -1,    -1,   413,   414,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   294,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   436,    -1,    -1,    -1,    -1,    -1,   442,   443,   444,
      -1,    -1,    -1,    -1,   336,    -1,   204,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   215,   463,    -1,
     336,    -1,    -1,    -1,    -1,    -1,   471,    -1,   473,    -1,
      -1,    -1,    -1,    -1,   366,    -1,   481,   482,   483,    -1,
      -1,    -1,    -1,    -1,    -1,   490,   491,   245,   364,    -1,
     366,    -1,    -1,    -1,   252,    -1,    -1,    -1,   256,    -1,
     258,    -1,    -1,   261,    -1,   263,    -1,    -1,    -1,    -1,
     402,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   276,    -1,
      -1,   413,   414,    -1,    -1,    -1,   402,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   294,   413,   414,    -1,
      -1,    -1,    -1,    -1,   436,    -1,    -1,    -1,    -1,    -1,
     442,   443,   444,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     436,    -1,    -1,    -1,    -1,    -1,   442,   443,   444,    -1,
      -1,   463,    -1,    -1,    -1,    -1,    -1,    -1,   336,   471,
      -1,   473,    -1,    -1,    -1,    -1,    -1,   463,    -1,   481,
     482,   483,    -1,    -1,    -1,   471,   472,   473,   490,   491,
      -1,    -1,    -1,    -1,    -1,   481,    -1,    -1,   366,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    42,    43,    44,    45,
      46,    47,    48,    49,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   402,    -1,    -1,    -1,    74,    75,
      76,    77,    -1,    -1,    -1,   413,   414,    83,    -1,    85,
      86,    87,    88,    89,    90,    91,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   436,    -1,
      -1,    -1,    -1,    -1,   442,   443,   444,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   128,   129,    -1,   463,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   471,    -1,   473,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   481,   482,   483,    -1,    -1,    -1,    -1,
      -1,    -1,   490,    22,    -1,    -1,    -1,    92,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    35,    -1,    -1,    -1,
      -1,    -1,   107,    42,    43,    44,    45,    46,    47,    48,
      49,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   126,    -1,    -1,    -1,   130,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   212,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    83,   221,    85,    86,    87,    88,
      89,    90,    91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   237,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   248,    -1,    -1,    -1,    -1,   253,   254,    -1,
      31,    32,    -1,    34,    -1,    -1,    37,    -1,    -1,   128,
     129,    -1,    -1,   198,    -1,    -1,    -1,    -1,   274,   204,
      -1,    52,    -1,    -1,   280,    -1,    -1,    -1,    -1,    -1,
     215,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     296,    -1,    -1,    -1,    -1,    76,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   311,    -1,    -1,    -1,    -1,
     245,   317,    -1,   319,    -1,    -1,   322,   252,   187,   100,
      -1,   256,    -1,   258,    -1,    -1,   261,   333,   263,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   208,
      -1,   276,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   221,    -1,    -1,    -1,   137,    -1,    -1,   294,
      -1,    -1,    -1,    -1,    -1,   234,    -1,    -1,   237,    -1,
      -1,    -1,    -1,    -1,   243,    -1,    -1,    -1,    -1,   248,
      -1,    -1,    -1,    -1,    -1,   391,    -1,   393,   394,   395,
      -1,    -1,    -1,    -1,   400,    -1,   177,    -1,    -1,    -1,
      -1,   336,    -1,    -1,    -1,   274,    -1,    -1,    -1,    -1,
      -1,   417,    -1,    -1,    -1,    -1,   197,    -1,    -1,    -1,
     201,    -1,    -1,    -1,    -1,    -1,    -1,   296,    -1,    -1,
      -1,   366,    -1,   214,    -1,    -1,    -1,    92,    -1,   445,
      -1,    -1,   311,    -1,    -1,   451,   452,   453,   317,    -1,
     319,    92,   107,    -1,    -1,    -1,    -1,    -1,   464,    -1,
      -1,   467,   468,    -1,   333,    -1,   107,   402,    -1,    -1,
      -1,   126,   253,    -1,    -1,   130,    -1,   346,   413,   414,
     261,   262,   488,    -1,   353,   126,    -1,    -1,    -1,   130,
     425,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     281,   436,    -1,    -1,    -1,    -1,    -1,   442,   443,   444,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     455,    -1,   391,    -1,   393,   394,   395,    -1,   463,   310,
      -1,    -1,    -1,   468,    -1,    92,   471,   472,   473,   320,
      -1,    -1,    -1,   198,    -1,    -1,   481,    -1,    -1,   204,
     107,   420,    -1,   334,    -1,    -1,    -1,    -1,    -1,    -1,
     215,    -1,    -1,   204,    -1,    -1,    -1,    -1,    -1,   126,
     439,    -1,    -1,   130,   215,    -1,    -1,   446,    -1,   360,
      -1,    -1,   451,   452,   453,    -1,   367,    -1,    -1,    -1,
     245,    -1,    -1,    -1,    -1,   464,   377,   252,   467,    -1,
     381,   256,    -1,   258,   245,    -1,   261,    -1,   263,    -1,
      -1,   252,    -1,    -1,    -1,   256,    -1,   258,    -1,    -1,
     261,   276,   263,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   276,    -1,    -1,    -1,   294,
     421,    -1,    -1,    -1,    -1,    -1,    -1,   204,    -1,   430,
      -1,    -1,    -1,   294,    -1,    -1,    -1,    -1,   215,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   447,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   457,    -1,    -1,    -1,
      -1,   336,   463,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   472,    -1,    -1,    -1,   336,    -1,    -1,    -1,   256,
      -1,   258,    -1,    -1,   261,    -1,   263,    -1,    -1,    -1,
      -1,   366,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   276,
      -1,    -1,    -1,    -1,    -1,   366,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   294,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   402,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   413,   414,
      -1,   402,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     425,    -1,   413,   414,    -1,    -1,    -1,    -1,    -1,   336,
      -1,   436,    -1,    -1,    -1,    -1,    -1,   442,   443,   444,
      -1,    -1,    -1,    -1,    -1,   436,    -1,    -1,    -1,    -1,
     455,   442,   443,   444,    -1,    -1,    -1,    -1,   463,   366,
      -1,    -1,    -1,   468,    -1,    -1,   471,   472,   473,    -1,
      -1,    -1,   463,    -1,    -1,    -1,   481,    -1,    -1,    -1,
     471,    -1,   473,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     481,    -1,    -1,   139,    -1,   402,    -1,   143,   144,   145,
     146,   147,   148,   149,   150,   151,   413,   414,   154,   155,
     156,    -1,   158,   159,   160,   161,   162,   163,   164,   165,
     166,   167,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   436,
      -1,    -1,    -1,    -1,    -1,   442,   443,   444,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   463,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   471,    -1,   473,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   481,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   283,   284,   285,
     286,   287,   288,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   470,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   488
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint16 yystos[] =
{
       0,   497,   498,     0,   203,   331,   499,   500,   507,   488,
     511,   511,   500,   518,   510,   508,    23,    93,   169,   190,
     228,   236,   271,   295,   359,   374,   383,   401,   403,   501,
     519,   520,   521,   523,   524,   525,   530,   538,   543,   577,
     578,   579,   580,   581,   610,   611,   612,   613,   614,   615,
     617,   621,   133,   256,   332,   513,   513,   102,   344,   354,
     477,   622,   380,   132,   488,   380,   488,   434,  1245,   488,
     488,   341,   398,   399,   616,   282,   303,  1225,   488,   488,
     623,   626,   521,   524,   578,   580,   611,   488,   614,    28,
     514,   514,   432,  1277,   300,  1259,   488,   488,   488,   423,
    1273,   265,   330,   382,   473,   528,   529,   531,   532,   533,
     534,   536,   537,  1263,   202,   539,   540,   541,    25,  1235,
     303,   473,  1184,   472,   526,   527,   528,  1279,    12,    69,
     105,   106,   108,   116,   181,   257,   292,   376,   391,   419,
     473,   544,   545,   546,   547,   548,   549,   553,   562,   567,
     570,   571,   572,   573,   574,   575,   576,   502,   110,   189,
     255,   259,   378,   434,   475,   624,   625,   627,   628,   629,
     632,   660,   712,   714,   256,   511,   509,   473,  1201,  1202,
    1204,   473,  1172,  1173,   522,    95,  1242,   396,   234,  1250,
     488,   532,   473,   488,   533,    73,   387,  1231,    10,   473,
     542,   488,   541,   196,  1246,   582,   488,   488,   527,   115,
    1184,  1184,   409,   391,  1269,  1250,  1250,  1250,  1184,   391,
    1270,  1250,   234,    67,  1241,   256,  1250,   488,   546,   243,
     439,   327,   770,   132,   380,   380,   380,   380,   356,   716,
     629,   630,   515,  1250,   220,   297,  1228,   133,   490,  1205,
    1206,  1173,   618,   619,  1173,  1250,   256,  1207,   488,   387,
    1250,   231,   231,   473,  1249,  1172,   583,   488,   269,   554,
    1250,  1250,  1250,  1181,  1204,    78,  1181,  1250,   234,  1181,
     563,   564,   565,  1184,  1250,   105,  1184,  1185,   386,  1227,
    1227,   132,   500,   503,   511,   488,   488,   488,   511,   380,
     375,   758,   631,   633,  1173,   511,    82,   187,   345,   435,
     516,   517,  1204,    10,    39,    92,   107,   126,   130,   204,
     215,   245,   252,   256,   258,   261,   263,   276,   294,   336,
     366,   402,   413,   414,   436,   442,   443,   444,   463,   471,
     481,   482,   483,   490,  1162,  1163,  1166,  1167,  1190,  1201,
    1210,  1211,  1212,   490,  1206,   619,   320,   620,  1207,  1181,
       5,    20,    30,    42,   222,   253,   260,   306,   312,   341,
     349,   361,   390,   398,   434,   488,   584,   585,   589,   591,
     592,   593,   594,   595,   599,   600,   601,   602,   603,   604,
     606,   607,   608,  1231,  1238,  1250,   568,   569,  1211,  1181,
     256,  1181,   563,    24,   234,  1184,  1251,  1184,   550,    66,
    1240,   268,   829,  1175,   153,   157,   500,   504,   715,   713,
     661,   488,   380,   662,   663,   634,   517,  1263,  1166,   490,
     490,  1213,   490,   490,  1214,   297,  1258,  1228,   490,   490,
     490,   490,   490,   490,   490,   490,   490,   490,   490,  1213,
    1166,  1166,  1166,    80,   384,   491,  1165,    38,    40,    41,
     482,   483,   484,   485,   487,   494,   495,  1166,  1207,    67,
     474,   535,   269,  1256,   341,  1265,  1277,     8,   388,  1256,
    1250,  1240,   121,   239,  1238,  1252,  1252,   281,  1207,  1279,
     409,  1250,   388,   126,   138,   215,   256,   263,   277,   336,
     402,   405,   406,   481,   555,   556,   557,   560,   569,   428,
     566,  1207,   551,    64,   466,   773,   506,   505,   323,   992,
     994,   995,   662,   662,   662,   717,   488,   664,  1219,   635,
    1164,  1166,  1166,  1164,  1164,  1215,  1202,  1210,  1212,   473,
    1166,  1218,  1166,  1164,  1166,  1217,  1166,  1164,  1164,  1166,
    1216,  1166,  1166,  1166,   491,  1163,  1166,  1166,  1166,  1166,
    1166,  1166,  1166,  1166,  1166,   491,  1166,  1211,  1250,  1252,
     136,   187,   587,  1250,   253,   341,   349,   601,  1250,  1250,
     388,  1250,  1250,  1235,    10,   281,   339,   609,  1250,   473,
     557,    18,   428,  1211,  1207,   298,   300,   552,   775,   774,
     364,   782,   513,  1264,  1264,   993,   992,   511,   338,   718,
     719,   720,   759,  1219,   256,   473,   665,   668,   673,    53,
      72,   110,   240,   251,   341,   342,   356,   358,   467,   488,
     636,   637,   639,   643,   646,   649,   651,   657,   658,   659,
    1250,   491,  1165,   494,   491,   491,  1165,   491,   491,   491,
    1165,   491,   491,   491,   491,  1165,   491,   491,   491,   491,
     491,   136,   337,   388,   590,  1250,   127,   253,   325,   586,
     599,    33,   183,   264,   596,   256,  1181,  1183,   405,   473,
     605,  1181,  1184,  1181,   307,  1262,  1262,   301,  1181,   558,
     560,   409,  1272,    55,   776,   777,   778,  1239,   776,  1204,
     488,   488,   488,   256,   473,   757,   720,   760,   665,   192,
     473,   669,   670,   488,   512,  1242,  1250,   650,   341,   343,
     645,  1229,  1250,  1242,  1250,  1256,  1250,    24,  1234,   297,
     187,   208,  1213,  1166,   491,  1166,  1213,  1181,  1213,  1213,
    1213,  1166,  1213,  1213,  1213,   243,   439,  1213,  1213,  1213,
    1213,   605,   128,   256,   588,  1204,   472,   597,   597,   135,
     472,  1221,  1230,   400,   489,  1181,  1182,   126,   215,   256,
     263,   336,   402,   481,   559,   561,  1250,   777,   396,   450,
     780,   124,   348,   467,   771,   256,  1200,   721,   668,   673,
     761,   762,   763,   666,    94,   488,  1207,   473,  1229,  1234,
    1250,  1234,  1250,   299,   404,   644,  1183,  1207,   468,  1250,
     757,   757,   191,   216,   473,   647,   491,  1221,   260,   371,
     135,  1250,  1180,  1181,    18,  1184,  1250,   396,   303,   781,
     117,   783,    71,    98,    99,   313,   488,   722,   723,   732,
    1250,   669,   763,   674,   671,  1250,   432,   642,  1180,   254,
    1255,   432,  1241,   220,  1248,   473,  1250,  1250,   300,  1180,
    1181,   561,    32,   118,  1207,  1250,   473,   488,   772,  1250,
    1250,  1234,   249,   250,   726,   736,   208,   764,    22,    35,
      42,    43,    44,    45,    46,    47,    48,    49,    83,    85,
      86,    87,    88,    89,    90,    91,   128,   129,   221,   237,
     248,   274,   296,   311,   317,   319,   333,   346,   353,   393,
     394,   395,   420,   451,   452,   453,   464,   467,   667,   675,
     676,   677,   678,   680,   681,   682,   683,   684,   685,   698,
     699,   701,   702,   705,   710,   711,  1250,  1270,    28,  1236,
     208,  1207,    67,   343,   638,   652,  1207,   396,  1271,   256,
     648,  1204,   648,   271,   341,   343,   598,  1207,   784,   785,
    1196,  1201,  1211,  1212,   193,   724,   725,   724,  1251,   668,
     737,   765,   245,   392,   450,   392,   450,   392,   450,   392,
     450,   392,   450,   470,  1278,   370,  1267,  1201,  1207,  1202,
    1204,   244,   370,   700,  1250,  1251,   512,   187,   208,   446,
     243,   439,    10,    56,   245,   672,  1209,  1210,   653,   654,
     655,   656,  1255,  1279,  1241,   200,   641,  1247,   598,   785,
       1,   473,   488,   786,   787,   788,   789,   790,   791,   797,
     798,   992,   731,   735,  1207,   473,   669,    32,    34,    37,
      50,    51,    52,    76,    98,   177,   197,   200,   201,   214,
     253,   261,   262,   281,   296,   310,   334,   360,   367,   381,
     396,   421,   432,   447,   463,   466,   467,   488,   681,   682,
     684,   698,   701,   703,   707,   708,   766,   769,  1201,  1209,
     481,   432,   689,   428,   473,   683,   707,    28,   679,  1227,
    1227,  1211,  1258,  1258,    31,  1237,   195,   642,  1207,   142,
     488,   473,   488,   380,     4,     6,    11,    19,    57,    58,
      63,    68,    70,    81,    84,    97,   119,   128,   131,   168,
     180,   184,   185,   199,   206,   209,   210,   217,   224,   226,
     229,   266,   270,   272,   280,   302,   316,   339,   340,   350,
     362,   363,   369,   371,   379,   389,   398,   407,   411,   412,
     415,   418,   424,   441,   449,   454,   465,   476,   478,   488,
     799,   800,   810,   815,   819,   823,   825,   847,   850,   858,
     863,   868,   869,   870,   874,   880,   884,   892,   907,   912,
     921,   923,   925,   928,   931,   937,   946,   948,   965,   967,
     970,   974,   981,   992,   996,  1004,  1005,  1006,  1008,  1011,
    1015,  1016,  1027,  1038,  1048,  1054,  1058,  1064,  1068,  1070,
    1072,  1074,  1077,  1088,  1089,  1098,  1100,  1101,  1142,   799,
    1201,   733,    75,    77,   253,   254,   727,   728,   738,  1250,
     290,  1257,  1250,   172,   174,  1250,  1190,  1257,    37,  1207,
    1240,  1250,  1201,  1201,  1251,   708,   428,  1207,   431,  1276,
    1204,   706,   256,  1202,  1202,    54,   438,  1255,  1241,   117,
     256,   792,   801,   811,   816,   820,   824,   848,   826,   851,
     859,   864,   434,   871,   875,   881,   885,   893,   908,   913,
     922,   924,  1277,   929,   932,   938,   947,   949,   966,   968,
     971,   385,   975,   982,   997,   440,  1007,   440,  1009,  1012,
    1017,  1028,  1039,  1049,  1055,  1059,  1065,   326,  1069,  1071,
    1073,  1075,  1078,   208,  1090,  1246,  1102,  1143,   213,   241,
     734,  1207,    74,    75,    76,    77,   212,   253,   254,   280,
     322,   400,   417,   445,   468,   488,   681,   682,   684,   685,
     698,   701,   705,   739,   740,   742,   743,   744,   745,   747,
     748,   749,   750,   754,   755,   481,  1198,  1201,  1207,  1250,
    1201,  1198,  1250,  1276,  1250,  1198,   707,  1209,    29,   122,
     123,   222,   686,   687,   688,   690,   691,   692,   693,   694,
     695,  1278,  1183,  1183,  1237,   122,   640,   488,   488,   802,
    1201,     7,   103,   245,   812,  1167,  1188,  1189,  1201,  1209,
    1212,   473,   817,  1152,  1153,  1154,   821,   829,   849,   829,
     852,  1195,  1196,   860,   865,  1168,  1169,  1189,   872,  1173,
      31,   458,   803,   876,   877,   878,  1189,  1237,  1279,   882,
    1189,   256,   433,   437,   894,   895,  1152,   314,   316,   330,
     380,   909,    65,   273,   916,   917,     7,  1186,  1187,  1201,
    1201,   926,   207,   364,   930,  1266,     1,   933,  1151,  1152,
    1186,   731,   950,  1201,  1209,  1212,  1040,  1204,   103,   969,
    1189,   972,  1189,   976,   198,   256,   473,   983,   986,   987,
     988,  1177,  1178,  1179,  1190,  1203,  1279,  1173,  1170,  1204,
    1173,  1170,    10,  1018,  1171,  1204,   169,  1029,  1030,  1031,
    1032,  1034,  1035,  1036,  1037,  1174,  1175,  1186,  1040,  1173,
     373,   411,  1056,   120,  1060,  1061,  1189,   103,  1066,  1188,
     731,  1201,  1173,  1196,     9,    36,  1092,   115,  1170,   206,
     315,  1144,  1250,    62,    98,   114,   125,   729,   730,   728,
     290,   291,   291,  1257,   223,   756,   290,   291,   753,  1234,
     212,   470,  1250,  1258,  1250,  1201,   745,   280,   318,   751,
     752,  1207,   267,   318,   482,   483,   768,   267,   318,   482,
     483,   767,  1200,   704,  1259,  1239,   688,   692,  1252,   389,
    1268,  1255,  1255,  1183,  1259,   143,   809,   200,   430,   803,
     806,   807,   808,  1279,  1258,  1201,  1258,   144,   814,   432,
     813,  1189,   225,  1222,    67,    13,    14,    15,    21,    38,
      39,    40,    41,   175,   176,   188,   205,   211,   234,   242,
     247,   278,   279,   282,   292,   299,   304,   321,   416,   482,
     483,   484,   485,   487,   489,   490,   491,   492,   493,  1155,
    1156,  1157,  1158,  1159,  1189,   256,   473,  1177,  1196,  1196,
    1196,   853,  1173,   146,   866,   175,   489,   867,  1169,   372,
    1226,  1173,  1265,  1193,  1201,  1210,  1212,   105,   325,   473,
    1175,   149,   879,   182,   878,  1109,  1111,  1189,   459,   460,
     461,   462,    76,   253,   320,   804,   805,    34,    37,    50,
      51,    52,   100,   177,   197,   214,   262,   281,   289,   310,
     367,   377,   447,   150,   883,    55,   230,   829,    18,   470,
     896,   897,   898,   901,   109,   910,   915,   917,  1189,  1258,
    1187,  1277,  1176,  1189,   427,  1275,   939,  1279,   101,   355,
     422,   951,   952,   953,   957,   962,  1042,  1189,   432,   155,
     973,    55,   186,   227,   235,   308,   977,  1228,   987,   156,
     984,   985,   455,   468,   428,  1228,   431,   425,   280,   324,
    1223,   200,  1103,  1265,  1103,  1171,   161,  1026,   468,  1020,
    1193,  1035,  1037,  1186,   432,  1175,   134,   432,   456,  1033,
     219,   999,  1279,   207,   364,  1057,  1189,     3,   256,  1239,
     230,  1061,  1201,   164,  1067,   200,   200,   341,   343,  1076,
     120,  1079,   357,   404,  1093,  1259,  1103,  1201,  1196,  1110,
    1111,  1207,   213,  1250,  1250,  1250,  1251,  1250,  1151,  1201,
     746,  1181,    98,   125,   313,   356,   741,   200,   313,  1207,
     752,   256,   481,  1197,  1199,  1201,  1208,  1197,  1278,  1181,
     473,   696,   697,  1250,  1277,  1255,  1181,    26,    27,    60,
      61,    77,    79,   111,   112,   113,   169,   171,   179,   254,
     429,   473,  1175,  1092,  1108,  1111,   808,    32,    34,    37,
      52,   100,   137,   177,   197,   201,   214,   261,   262,   281,
     310,   334,   360,   367,   377,   381,   421,   447,   457,   463,
     168,   330,  1191,  1202,   432,  1202,  1210,  1212,  1168,  1189,
     207,   364,   818,  1222,  1277,   426,  1274,  1274,   205,   242,
     278,  1157,  1158,  1159,  1156,  1228,   432,   408,   410,   472,
     830,   830,    97,   184,   855,   347,   448,   861,   862,  1279,
    1152,   148,   873,   233,   286,  1136,  1137,  1138,  1140,  1112,
     285,  1113,  1109,  1109,  1109,  1109,  1257,  1257,  1257,  1250,
     172,   174,  1250,    37,   134,   456,  1189,  1168,  1189,   466,
     886,   887,   895,    22,   307,   433,   437,   902,   903,   904,
    1153,   151,   906,   898,   470,   899,   911,  1266,   458,   918,
     919,  1189,  1279,  1202,  1187,   122,   927,  1177,   934,    10,
      13,    16,    17,   274,   275,   292,   293,   940,   944,   192,
      10,  1233,    10,    67,   194,   243,   439,   958,   959,   960,
     954,   952,  1044,  1230,  1259,   432,  1186,  1168,  1189,   390,
     978,  1203,   793,   794,   184,   989,  1151,   990,   991,  1201,
    1177,  1179,     9,    36,  1105,  1265,  1190,   230,   998,  1013,
    1279,  1021,  1237,  1201,  1021,   432,   432,   552,   168,   433,
     437,  1189,    55,   260,  1050,   218,   238,   260,   281,   469,
    1189,  1189,  1057,   396,  1189,  1201,   200,  1168,  1189,  1193,
    1239,   230,  1082,  1201,   178,   182,  1094,    10,  1099,  1177,
    1013,   200,  1279,   167,  1147,  1113,   194,  1207,  1207,  1207,
    1207,  1226,   195,   213,   195,   213,   195,   213,  1189,  1268,
     697,  1180,   433,   709,  1108,   480,   479,  1193,  1108,   239,
    1198,  1113,  1250,   172,   174,  1250,    37,    66,   134,   456,
     256,  1192,  1202,  1201,   397,  1116,  1117,  1168,  1187,   364,
     329,   822,    59,   410,   831,   255,  1253,   408,   832,   827,
     466,   833,   834,   364,   472,   856,  1196,   854,  1246,   260,
     281,  1116,  1139,  1141,   873,  1140,  1115,  1201,  1114,  1190,
    1190,  1190,  1198,  1198,  1150,  1239,  1150,   207,  1116,   207,
     888,   364,   891,    18,   428,   905,   902,   793,   900,  1189,
     105,   325,   473,  1175,   914,   919,   289,  1259,   139,   154,
     793,   935,  1277,   355,   941,  1277,  1193,    55,   959,   961,
    1193,    10,    67,   243,   439,   955,   956,  1193,  1045,  1231,
     220,   305,  1261,   694,  1186,  1116,   207,  1279,  1172,   985,
     795,     9,   200,   219,   472,  1000,  1201,   142,   283,  1126,
    1127,  1129,  1136,   260,   281,   470,   142,   470,  1023,  1024,
    1193,  1192,  1189,   239,  1051,   260,   260,   260,  1062,  1279,
    1201,  1116,   207,   432,    10,  1080,  1081,  1220,  1083,  1201,
    1062,  1083,   327,  1097,   328,  1104,  1105,  1196,   141,  1149,
     729,   195,    55,  1277,  1250,  1108,  1198,  1198,  1250,  1150,
    1150,  1226,  1118,   288,  1119,  1116,  1187,   432,  1177,  1253,
     831,   182,   207,   309,   364,   828,   834,   839,   840,   841,
    1095,   835,   841,  1196,  1196,   207,   857,   352,   368,   793,
     793,   793,   793,    78,  1243,  1243,  1243,  1198,  1168,  1169,
    1168,  1169,   838,   889,   890,  1239,  1204,   904,  1152,   152,
     920,  1196,   154,   793,   139,   154,   936,   467,   942,   943,
     944,   118,   945,   467,   432,  1193,    55,   963,   956,   196,
     963,  1041,  1250,   305,  1252,  1168,   609,   365,   980,   797,
     991,  1189,   260,   218,   238,   260,   281,   469,   998,  1128,
    1130,   159,  1010,  1129,   160,  1014,   260,  1152,  1022,  1151,
    1023,  1250,  1136,   309,  1121,  1122,   319,  1226,  1168,  1194,
    1201,  1209,  1212,   304,  1260,  1193,   121,  1084,   422,  1086,
    1259,   173,   284,  1106,  1131,  1132,  1134,  1137,     8,  1232,
     104,  1146,  1196,   840,  1148,  1250,  1250,  1189,   709,  1209,
     256,  1116,   793,  1120,  1253,     7,   230,  1201,   285,   287,
     843,  1096,   842,   836,   837,   838,   843,  1201,   253,   254,
    1254,  1116,   351,  1116,   351,   299,   780,   890,    96,   124,
     348,   467,   936,   154,   793,   943,   110,  1244,  1194,   963,
    1194,  1105,   227,   466,  1046,  1181,  1043,  1116,   979,  1279,
      55,   260,   260,   260,   999,   793,   793,  1019,   793,  1025,
     282,  1052,  1224,   162,  1053,  1123,   163,  1063,   287,  1124,
    1201,  1116,  1116,  1081,  1248,   104,  1085,  1248,  1121,   186,
     227,   235,   308,  1091,  1172,  1133,  1135,   166,  1107,  1134,
     313,  1175,  1198,  1248,   274,   327,   335,  1145,  1207,  1207,
     793,  1258,  1201,   145,   845,   844,   793,   837,   299,   780,
     147,   846,  1169,  1169,    22,  1204,   936,    55,   963,   963,
     435,   964,   327,  1172,   207,   308,  1047,  1204,   260,   281,
    1189,   239,  1001,   793,   796,   796,  1189,   205,   211,   242,
     247,  1157,  1158,  1159,  1160,  1161,   793,  1125,  1201,  1248,
    1201,   165,  1087,   793,   793,  1254,  1201,   327,  1250,  1201,
     793,  1189,  1116,  1116,   780,   780,  1189,  1189,  1250,  1172,
     327,   368,   455,  1250,  1002,  1126,  1137,  1274,  1274,   793,
    1201,   986,   780,   120,   779,   986,  1250,  1151,  1201,   158,
    1003,   304,   304,   779,   986,   175,   175,  1277,  1277
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK (1);						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
	      (Loc).first_line, (Loc).first_column,	\
	      (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
#else
static void
yy_stack_print (yybottom, yytop)
    yytype_int16 *yybottom;
    yytype_int16 *yytop;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, int yyrule)
#else
static void
yy_reduce_print (yyvsp, yyrule)
    YYSTYPE *yyvsp;
    int yyrule;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, Rule); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
	switch (*++yyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++yyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (yyres)
	      yyres[yyn] = *yyp;
	    yyn++;
	    break;

	  case '"':
	    if (yyres)
	      yyres[yyn] = '\0';
	    return yyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into YYRESULT an error message about the unexpected token
   YYCHAR while in state YYSTATE.  Return the number of bytes copied,
   including the terminating null byte.  If YYRESULT is null, do not
   copy anything; just return the number of bytes that would be
   copied.  As a special case, return 0 if an ordinary "syntax error"
   message will do.  Return YYSIZE_MAXIMUM if overflow occurs during
   size calculation.  */
static YYSIZE_T
yysyntax_error (char *yyresult, int yystate, int yychar)
{
  int yyn = yypact[yystate];

  if (! (YYPACT_NINF < yyn && yyn <= YYLAST))
    return 0;
  else
    {
      int yytype = YYTRANSLATE (yychar);
      YYSIZE_T yysize0 = yytnamerr (0, yytname[yytype]);
      YYSIZE_T yysize = yysize0;
      YYSIZE_T yysize1;
      int yysize_overflow = 0;
      enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
      char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
      int yyx;

# if 0
      /* This is so xgettext sees the translatable formats that are
	 constructed on the fly.  */
      YY_("syntax error, unexpected %s");
      YY_("syntax error, unexpected %s, expecting %s");
      YY_("syntax error, unexpected %s, expecting %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
      char *yyfmt;
      char const *yyf;
      static char const yyunexpected[] = "syntax error, unexpected %s";
      static char const yyexpecting[] = ", expecting %s";
      static char const yyor[] = " or %s";
      char yyformat[sizeof yyunexpected
		    + sizeof yyexpecting - 1
		    + ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
		       * (sizeof yyor - 1))];
      char const *yyprefix = yyexpecting;

      /* Start YYX at -YYN if negative to avoid negative indexes in
	 YYCHECK.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;

      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yycount = 1;

      yyarg[0] = yytname[yytype];
      yyfmt = yystpcpy (yyformat, yyunexpected);

      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	  {
	    if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
	      {
		yycount = 1;
		yysize = yysize0;
		yyformat[sizeof yyunexpected - 1] = '\0';
		break;
	      }
	    yyarg[yycount++] = yytname[yyx];
	    yysize1 = yysize + yytnamerr (0, yytname[yyx]);
	    yysize_overflow |= (yysize1 < yysize);
	    yysize = yysize1;
	    yyfmt = yystpcpy (yyfmt, yyprefix);
	    yyprefix = yyor;
	  }

      yyf = YY_(yyformat);
      yysize1 = yysize + yystrlen (yyf);
      yysize_overflow |= (yysize1 < yysize);
      yysize = yysize1;

      if (yysize_overflow)
	return YYSIZE_MAXIMUM;

      if (yyresult)
	{
	  /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
	  char *yyp = yyresult;
	  int yyi = 0;
	  while ((*yyp = *yyf) != '\0')
	    {
	      if (*yyp == '%' && yyf[1] == 's' && yyi < yycount)
		{
		  yyp += yytnamerr (yyp, yyarg[yyi++]);
		  yyf += 2;
		}
	      else
		{
		  yyp++;
		  yyf++;
		}
	    }
	}
      return yysize;
    }
}
#endif /* YYERROR_VERBOSE */


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yymsg, yytype, yyvaluep)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  YYUSE (yyvaluep);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
	break;
    }
}

/* Prevent warnings from -Wmissing-prototypes.  */
#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */


/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*-------------------------.
| yyparse or yypush_parse.  |
`-------------------------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{


    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       `yyss': related to states.
       `yyvs': related to semantic values.

       Refer to the stacks thru separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yytoken = 0;
  yyss = yyssa;
  yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */
  yyssp = yyss;
  yyvsp = yyvs;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	yytype_int16 *yyss1 = yyss;

	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),
		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	yytype_int16 *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyexhaustedlab;
	YYSTACK_RELOCATE (yyss_alloc, yyss);
	YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  *++yyvsp = yylval;

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:

/* Line 1455 of yacc.c  */
#line 900 "parser.y"
    {
        perform_stack = NULL;
        current_statement = NULL;
        /*CIT*/
        current_paragraph = NULL;
        current_section = NULL;

        next_label_id = 0;
        current_linage = 0;
        current_storage = 0;
        eval_level = 0;
        eval_inc = 0;
        eval_inc2 = 0;
        prog_end = 0;
        depth = 0;
        inspect_keyword = 0;
        check_unreached = 0;
        samearea = 1;
        memset ((char *)eval_check, 0, sizeof(eval_check));
        memset ((char *)term_array, 0, sizeof(term_array));
        linage_file = NULL;
        next_label_list = NULL;
        current_program = cb_build_program (NULL, 0);
        external_program_list = NULL;
        cb_build_registers ();
        current_program->flag_main = cb_flag_main;
  ;}
    break;

  case 3:

/* Line 1455 of yacc.c  */
#line 929 "parser.y"
    {
    if (!current_program->flag_validated) {
        current_program->flag_validated = 1;
                cb_validate_program_body (current_program);
        }
    if (depth > 1) {
                cb_error (_("Multiple PROGRAM-ID's without matching END PROGRAM"));
        }
        if (errorcount > 0) {
                YYABORT;
        }
        if (!current_program->entry_list) {
                emit_entry (current_program->program_id, 0, NULL, 0, 1);
        }
  ;}
    break;

  case 6:

/* Line 1455 of yacc.c  */
#line 953 "parser.y"
    { cb_validate_program_environment (current_program); ;}
    break;

  case 7:

/* Line 1455 of yacc.c  */
#line 954 "parser.y"
    { cb_validate_program_data (current_program); ;}
    break;

  case 8:

/* Line 1455 of yacc.c  */
#line 959 "parser.y"
    {
    if (!current_program->flag_validated) {
        current_program->flag_validated = 1;
        cb_validate_program_body (current_program);
    }
  ;}
    break;

  case 13:

/* Line 1455 of yacc.c  */
#line 974 "parser.y"
    {
    cb_force_pid_literal = 1;
  ;}
    break;

  case 14:

/* Line 1455 of yacc.c  */
#line 978 "parser.y"
    {
    char                    *s;

    if ((yyvsp[(3) - (4)])) {
        if (CB_LITERAL_P ((yyvsp[(3) - (4)]))) {
                s = (char *)(CB_LITERAL ((yyvsp[(3) - (4)]))->data);
        } else {
                s = (char *)(CB_NAME ((yyvsp[(3) - (4)])));
        }
        if (depth) {
                depth--;
        }
        if (strcasecmp (stack_progid[depth], s)) {
            cb_error (_("END PROGRAM '%s' is different to PROGRAM-ID '%s'"),
                s, stack_progid[depth]);
        }
    }
  ;}
    break;

  case 15:

/* Line 1455 of yacc.c  */
#line 997 "parser.y"
    {
    cb_force_pid_literal = 1;
  ;}
    break;

  case 16:

/* Line 1455 of yacc.c  */
#line 1001 "parser.y"
    {
    char            *s;

    if ((yyvsp[(3) - (4)])) {
        if (CB_LITERAL_P ((yyvsp[(3) - (4)]))) {
            s = (char *)(CB_LITERAL ((yyvsp[(3) - (4)]))->data);
        } else {
            s = (char *)(CB_NAME ((yyvsp[(3) - (4)])));
        }
        if (depth) {
            depth--;
        }
        if (strcmp (stack_progid[depth], s)) {
            cb_error (_("END FUNCTION '%s' is different to FUNCTION-ID '%s'"),
                s, stack_progid[depth]);
        }
    }
  ;}
    break;

  case 17:

/* Line 1455 of yacc.c  */
#line 1028 "parser.y"
    {
    cb_force_pid_literal = 1;
  ;}
    break;

  case 18:

/* Line 1455 of yacc.c  */
#line 1032 "parser.y"
    {
        char LocBuffer[1024];
        current_section = NULL;
        current_paragraph = NULL;
        if (CB_LITERAL_P ((yyvsp[(4) - (5)]))) {
            stack_progid[depth] = (char *)(CB_LITERAL ((yyvsp[(4) - (5)]))->data);
            } else {
            stack_progid[depth] = (char *)(CB_NAME ((yyvsp[(4) - (5)])));
        }
        if (prog_end) {
            if (!current_program->flag_validated) {
                current_program->flag_validated = 1;
                cb_validate_program_body (current_program);
            }
            perform_stack = NULL;
            current_statement = NULL;
            next_label_id = 0;
            current_linage = 0;
            current_storage = 0;
            eval_level = 0;
            inspect_keyword = 0;
            check_unreached = 0;
            eval_inc = 0;
            eval_inc2 = 0;
            samearea = 1;
            memset ((char *)eval_check, 0, sizeof(eval_check));
            memset ((char *)term_array, 0, sizeof(term_array));
            linage_file = NULL;
            next_label_list = NULL;
            if (current_program && current_program->flag_is_external) {
                struct cb_program *n =current_program->next_program;
                current_program->next_program = external_program_list;
                external_program_list = current_program;
                current_program = cb_build_program (n, depth);
            } else {
                current_program = cb_build_program (current_program, depth);
            }
            cb_build_registers ();
        } else {
          prog_end = 1;
        }
    depth++;
    current_program->program_id = cb_build_program_id ((yyvsp[(4) - (5)]), (yyvsp[(5) - (5)]));
    if (source_demangle_name && (strcasecmp(source_demangle_name, current_program->program_id) != 0)) {                
      sprintf(LocBuffer, "%s_%s_", current_program->program_id, source_demangle_name);
    } else {
      sprintf(LocBuffer, "%s_", current_program->program_id);
    }
    cb_disable_runtime_check=0;
    current_program->main_entry_name = (char*)strdup(LocBuffer);
    if ( cb_flag_debug_parser ) fprintf(stderr, "[DEBUG PARSER] PROGRAM_ID %s\n", current_program->program_id );
    current_program->source_file =  strdup((char*)cb_lex_source_file.file_name);
    current_program->source_line = cb_lex_source_file.line;
  ;}
    break;

  case 20:

/* Line 1455 of yacc.c  */
#line 1091 "parser.y"
    {
    cb_force_pid_literal = 1;
  ;}
    break;

  case 21:

/* Line 1455 of yacc.c  */
#line 1095 "parser.y"
    {
    cb_error (_("FUNCTION-ID is not yet implemented"));
    current_section = NULL;
    current_paragraph = NULL;
    if (CB_LITERAL_P ((yyvsp[(4) - (6)]))) {
        stack_progid[depth] = (char *)(CB_LITERAL ((yyvsp[(4) - (6)]))->data);
    } else {
        stack_progid[depth] = (char *)(CB_NAME ((yyvsp[(4) - (6)])));
    }
    if (prog_end) {
        if (!current_program->flag_validated) {
            current_program->flag_validated = 1;
            cb_validate_program_body (current_program);
        }
        perform_stack = NULL;
        current_statement = NULL;
        next_label_id = 0;
        current_linage = 0;
        current_storage = 0;
        eval_level = 0;
        inspect_keyword = 0;
        check_unreached = 0;
        eval_inc = 0;
        eval_inc2 = 0;
        samearea = 1;
        memset ((char *)eval_check, 0, sizeof(eval_check));
        memset ((char *)term_array, 0, sizeof(term_array));
        linage_file = NULL;
        next_label_list = NULL;
        current_program = cb_build_program (current_program, depth);
        cb_build_registers ();
    } else {
        prog_end = 1;
    }
    depth++;
    cb_disable_runtime_check=0;
    current_program->program_id = cb_build_program_id ((yyvsp[(4) - (6)]), (yyvsp[(5) - (6)]));
    current_program->prog_type = CB_FUNCTION_TYPE;
    current_program->flag_recursive = 1;
    current_program->flag_initial = 1;
    current_program->source_file =  strdup((char*)cb_lex_source_file.file_name);
    current_program->source_line = cb_lex_source_file.line;
  ;}
    break;

  case 28:

/* Line 1455 of yacc.c  */
#line 1151 "parser.y"
    {cb_force_pid_literal = 0;;}
    break;

  case 29:

/* Line 1455 of yacc.c  */
#line 1152 "parser.y"
    {
                   const char *p  = CB_REFERENCE (yylval)->word->name;
                   cb_force_pid_literal = 0;
                   (yyval) = cb_build_alphanumeric_literal ((const unsigned char *)p, strlen(p), 0);
                   ;}
    break;

  case 30:

/* Line 1455 of yacc.c  */
#line 1160 "parser.y"
    { (yyval) = NULL; ;}
    break;

  case 31:

/* Line 1455 of yacc.c  */
#line 1161 "parser.y"
    { (yyval) = (yyvsp[(2) - (2)]); ;}
    break;

  case 34:

/* Line 1455 of yacc.c  */
#line 1170 "parser.y"
    {
    if (!current_program->nested_level) {
        cb_error (_("COMMON may only be used in a nested program"));
    }
    current_program->flag_common = 1;
  ;}
    break;

  case 35:

/* Line 1455 of yacc.c  */
#line 1177 "parser.y"
    {
    if (!current_program->nested_level) {
        cb_error (_("COMMON may only be used in a nested program"));
    }
    current_program->flag_common = 1;
  ;}
    break;

  case 36:

/* Line 1455 of yacc.c  */
#line 1184 "parser.y"
    {
    current_program->flag_is_external = 1;
  ;}
    break;

  case 38:

/* Line 1455 of yacc.c  */
#line 1192 "parser.y"
    {
    current_program->flag_initial = 1;
  ;}
    break;

  case 39:

/* Line 1455 of yacc.c  */
#line 1196 "parser.y"
    {
    current_program->flag_recursive = 1;
    current_program->flag_initial = 1;
  ;}
    break;

  case 42:

/* Line 1455 of yacc.c  */
#line 1211 "parser.y"
    {
                                   if ( cb_flag_debug_parser ) fprintf(stderr, "[DEBUG PARSER] ENVIRONMENT DIVISION\n");
                           ;}
    break;

  case 47:

/* Line 1455 of yacc.c  */
#line 1227 "parser.y"
    {
                                   if ( cb_flag_debug_parser ) fprintf(stderr, "[DEBUG PARSER] CONFIGURATION SECTION\n");
                            ;}
    break;

  case 48:

/* Line 1455 of yacc.c  */
#line 1230 "parser.y"
    {
    if (current_program->nested_level) {
        cb_error (_("CONFIGURATION SECTION not allowed in nested programs"));
    }
  ;}
    break;

  case 49:

/* Line 1455 of yacc.c  */
#line 1236 "parser.y"
    {
    if (current_program->nested_level) {
        cb_error (_("CONFIGURATION SECTION not allowed in nested programs"));
    }
  ;}
    break;

  case 61:

/* Line 1455 of yacc.c  */
#line 1273 "parser.y"
    {
        cb_verify (cb_debugging_line, "DEBUGGING MODE");
  ;}
    break;

  case 62:

/* Line 1455 of yacc.c  */
#line 1279 "parser.y"
    { ;}
    break;

  case 63:

/* Line 1455 of yacc.c  */
#line 1283 "parser.y"
    { ;}
    break;

  case 64:

/* Line 1455 of yacc.c  */
#line 1284 "parser.y"
    { ;}
    break;

  case 75:

/* Line 1455 of yacc.c  */
#line 1314 "parser.y"
    {
        cb_verify (cb_memory_size_clause, "MEMORY SIZE");
  ;}
    break;

  case 78:

/* Line 1455 of yacc.c  */
#line 1326 "parser.y"
    {
    current_program->collating_sequence = (yyvsp[(4) - (4)]);
  ;}
    break;

  case 79:

/* Line 1455 of yacc.c  */
#line 1333 "parser.y"
    {
    /* Ignore */
  ;}
    break;

  case 85:

/* Line 1455 of yacc.c  */
#line 1358 "parser.y"
    {
    current_program->function_spec_list = (yyvsp[(2) - (3)]);
  ;}
    break;

  case 86:

/* Line 1455 of yacc.c  */
#line 1362 "parser.y"
    {
    functions_are_all = 1;
  ;}
    break;

  case 87:

/* Line 1455 of yacc.c  */
#line 1368 "parser.y"
    { (yyval) = cb_list_init ((yyvsp[(1) - (1)])); ;}
    break;

  case 88:

/* Line 1455 of yacc.c  */
#line 1370 "parser.y"
    { (yyval) = cb_list_add ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])); ;}
    break;

  case 106:

/* Line 1455 of yacc.c  */
#line 1411 "parser.y"
    {
        current_program->flag_sign_separate = CB_INTEGER ((yyvsp[(3) - (3)]))->val;
        current_program->flag_sign_leading  = 1;
  ;}
    break;

  case 107:

/* Line 1455 of yacc.c  */
#line 1416 "parser.y"
    {
        current_program->flag_sign_separate = CB_INTEGER ((yyvsp[(3) - (3)]))->val;
        current_program->flag_sign_leading  = 0;
  ;}
    break;

  case 111:

/* Line 1455 of yacc.c  */
#line 1426 "parser.y"
    {
    save_tree_1 = lookup_system_name (CB_NAME ((yyvsp[(1) - (3)])));
    if (save_tree_1 == cb_error_node) {
        cb_error_x ((yyvsp[(1) - (3)]), _("Unknown system-name '%s'"), CB_NAME ((yyvsp[(1) - (3)])));
    } else if (CB_SYSTEM_NAME(save_tree_1)->token != COB_DEVICE_CONSOLE) {
        cb_error_x (save_tree_1, _("Invalid CRT clause"));
    }
        /* current_program->flag_screen = 1; */
  ;}
    break;

  case 112:

/* Line 1455 of yacc.c  */
#line 1436 "parser.y"
    {
    save_tree_1 = lookup_system_name (CB_NAME ((yyvsp[(1) - (3)])));
    if (save_tree_1 == cb_error_node) {
        cb_error_x ((yyvsp[(1) - (3)]), _("Unknown system-name '%s'"), CB_NAME ((yyvsp[(1) - (3)])));
        save_tree_2 = cb_error_node;
    } else {
        if ((yyvsp[(3) - (3)])) {
            save_tree_2 = (yyvsp[(3) - (3)]);
            cb_define ((yyvsp[(3) - (3)]), save_tree_1);
        } else {
            save_tree_2 = (yyvsp[(1) - (3)]);
        }
    }
  ;}
    break;

  case 114:

/* Line 1455 of yacc.c  */
#line 1463 "parser.y"
    {
    save_tree_1 = lookup_system_name (CB_NAME ((yyvsp[(1) - (4)])));
    if (save_tree_1 == cb_error_node) {
        cb_error_x ((yyvsp[(1) - (4)]), _("Unknown system-name '%s'"), CB_NAME ((yyvsp[(1) - (4)])));
    } else if (CB_SYSTEM_NAME(save_tree_1)->category != CB_CALL_CONVENTION_NAME) {
        cb_error_x (save_tree_1, _("Invalid special-names clause"));
    }
    cb_define ((yyvsp[(4) - (4)]), cb_build_system_name(CB_SYSTEM_NAME(save_tree_1)->category, cb_get_int((yyvsp[(2) - (4)]))));
  ;}
    break;

  case 116:

/* Line 1455 of yacc.c  */
#line 1477 "parser.y"
    {
        if ((save_tree_1 != cb_error_node) && (save_tree_2 != cb_error_node))
            cb_define_switch_name ((yyvsp[(5) - (5)]), save_tree_1, (yyvsp[(2) - (5)]), save_tree_2);
    ;}
    break;

  case 117:

/* Line 1455 of yacc.c  */
#line 1484 "parser.y"
    { (yyval) = cb_int1; ;}
    break;

  case 118:

/* Line 1455 of yacc.c  */
#line 1485 "parser.y"
    { (yyval) = cb_int0; ;}
    break;

  case 119:

/* Line 1455 of yacc.c  */
#line 1493 "parser.y"
    {
        save_tree_1 = (yyvsp[(2) - (2)]);
  ;}
    break;

  case 120:

/* Line 1455 of yacc.c  */
#line 1497 "parser.y"
    {
        current_program->alphabet_name_list =
        cb_list_add (current_program->alphabet_name_list, (yyvsp[(5) - (5)]));
  ;}
    break;

  case 121:

/* Line 1455 of yacc.c  */
#line 1504 "parser.y"
    { (yyval) = cb_build_alphabet_name (save_tree_1, CB_ALPHABET_NATIVE); ;}
    break;

  case 122:

/* Line 1455 of yacc.c  */
#line 1505 "parser.y"
    { (yyval) = cb_build_alphabet_name (save_tree_1, CB_ALPHABET_STANDARD_1); ;}
    break;

  case 123:

/* Line 1455 of yacc.c  */
#line 1506 "parser.y"
    { (yyval) = cb_build_alphabet_name (save_tree_1, CB_ALPHABET_STANDARD_2); ;}
    break;

  case 124:

/* Line 1455 of yacc.c  */
#line 1507 "parser.y"
    { (yyval) = cb_build_alphabet_name (save_tree_1, CB_ALPHABET_EBCDIC); ;}
    break;

  case 125:

/* Line 1455 of yacc.c  */
#line 1509 "parser.y"
    {
    (yyval) = cb_build_alphabet_name (save_tree_1, CB_ALPHABET_CUSTOM);
        CB_ALPHABET_NAME ((yyval))->custom_list = (yyvsp[(1) - (1)]);
  ;}
    break;

  case 126:

/* Line 1455 of yacc.c  */
#line 1516 "parser.y"
    { (yyval) = cb_list_init ((yyvsp[(1) - (1)])); ;}
    break;

  case 127:

/* Line 1455 of yacc.c  */
#line 1518 "parser.y"
    { (yyval) = cb_list_add ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])); ;}
    break;

  case 128:

/* Line 1455 of yacc.c  */
#line 1522 "parser.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 129:

/* Line 1455 of yacc.c  */
#line 1523 "parser.y"
    { (yyval) = cb_build_pair ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])); ;}
    break;

  case 130:

/* Line 1455 of yacc.c  */
#line 1525 "parser.y"
    {
    (yyval) = cb_list_init ((yyvsp[(1) - (2)]));
    save_tree_2 = (yyval);
  ;}
    break;

  case 131:

/* Line 1455 of yacc.c  */
#line 1530 "parser.y"
    {
    (yyval) = (yyvsp[(3) - (4)]);
  ;}
    break;

  case 134:

/* Line 1455 of yacc.c  */
#line 1541 "parser.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 135:

/* Line 1455 of yacc.c  */
#line 1542 "parser.y"
    { (yyval) = cb_space; ;}
    break;

  case 136:

/* Line 1455 of yacc.c  */
#line 1543 "parser.y"
    { (yyval) = cb_zero; ;}
    break;

  case 137:

/* Line 1455 of yacc.c  */
#line 1544 "parser.y"
    { (yyval) = cb_quote; ;}
    break;

  case 138:

/* Line 1455 of yacc.c  */
#line 1545 "parser.y"
    { (yyval) = cb_dirsep; ;}
    break;

  case 139:

/* Line 1455 of yacc.c  */
#line 1546 "parser.y"
    { (yyval) = cb_norm_high; ;}
    break;

  case 140:

/* Line 1455 of yacc.c  */
#line 1547 "parser.y"
    { (yyval) = cb_norm_low; ;}
    break;

  case 141:

/* Line 1455 of yacc.c  */
#line 1551 "parser.y"
    { cb_list_add (save_tree_2, (yyvsp[(1) - (1)])); ;}
    break;

  case 142:

/* Line 1455 of yacc.c  */
#line 1552 "parser.y"
    { cb_list_add (save_tree_2, cb_space); ;}
    break;

  case 143:

/* Line 1455 of yacc.c  */
#line 1553 "parser.y"
    { cb_list_add (save_tree_2, cb_zero); ;}
    break;

  case 144:

/* Line 1455 of yacc.c  */
#line 1554 "parser.y"
    { cb_list_add (save_tree_2, cb_quote); ;}
    break;

  case 145:

/* Line 1455 of yacc.c  */
#line 1555 "parser.y"
    { cb_list_add (save_tree_2, cb_dirsep); ;}
    break;

  case 146:

/* Line 1455 of yacc.c  */
#line 1556 "parser.y"
    { cb_list_add (save_tree_2, cb_norm_high); ;}
    break;

  case 147:

/* Line 1455 of yacc.c  */
#line 1557 "parser.y"
    { cb_list_add (save_tree_2, cb_norm_low); ;}
    break;

  case 148:

/* Line 1455 of yacc.c  */
#line 1565 "parser.y"
    {
        /*PENDING ("SYMBOLIC CHARACTERS");*/
  ;}
    break;

  case 149:

/* Line 1455 of yacc.c  */
#line 1572 "parser.y"
    {
        if ((yyvsp[(1) - (1)])) {
            current_program-> symbolic_list =
                cb_list_append (current_program->symbolic_list, (yyvsp[(1) - (1)]));
        }
  ;}
    break;

  case 150:

/* Line 1455 of yacc.c  */
#line 1579 "parser.y"
    {
        if ((yyvsp[(1) - (2)])) {
            current_program-> symbolic_list =
                cb_list_append (current_program->symbolic_list, (yyvsp[(1) - (2)]));
        }
  ;}
    break;

  case 151:

/* Line 1455 of yacc.c  */
#line 1589 "parser.y"
    {
    if (cb_list_length ((yyvsp[(1) - (3)])) != cb_list_length ((yyvsp[(3) - (3)]))) {
        cb_error (_("Invalid SYMBOLIC clause"));
        (yyval) = NULL;
    } else {
        (yyval) = cb_list_join((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
    }
  ;}
    break;

  case 152:

/* Line 1455 of yacc.c  */
#line 1600 "parser.y"
    { (yyval) = cb_list_init ((yyvsp[(1) - (1)])); ;}
    break;

  case 153:

/* Line 1455 of yacc.c  */
#line 1601 "parser.y"
    { (yyval) = cb_list_add ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])); ;}
    break;

  case 154:

/* Line 1455 of yacc.c  */
#line 1605 "parser.y"
    { (yyval) = cb_list_init ((yyvsp[(1) - (1)])); ;}
    break;

  case 155:

/* Line 1455 of yacc.c  */
#line 1606 "parser.y"
    { (yyval) = cb_list_add ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])); ;}
    break;

  case 156:

/* Line 1455 of yacc.c  */
#line 1614 "parser.y"
    {
        current_program->class_name_list =
                        cb_list_add (current_program->class_name_list,
                        cb_build_class_name ((yyvsp[(2) - (4)]), (yyvsp[(4) - (4)])));
  ;}
    break;

  case 157:

/* Line 1455 of yacc.c  */
#line 1622 "parser.y"
    { (yyval) = cb_list_init ((yyvsp[(1) - (1)])); ;}
    break;

  case 158:

/* Line 1455 of yacc.c  */
#line 1623 "parser.y"
    { (yyval) = cb_list_add ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])); ;}
    break;

  case 159:

/* Line 1455 of yacc.c  */
#line 1627 "parser.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 160:

/* Line 1455 of yacc.c  */
#line 1629 "parser.y"
    {
        /* if (CB_LITERAL ($1)->data[0] < CB_LITERAL ($3)->data[0]) */
        if (literal_value ((yyvsp[(1) - (3)])) < literal_value ((yyvsp[(3) - (3)]))) {
                (yyval) = cb_build_pair ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
        } else {
                (yyval) = cb_build_pair ((yyvsp[(3) - (3)]), (yyvsp[(1) - (3)]));
        }
  ;}
    break;

  case 161:

/* Line 1455 of yacc.c  */
#line 1643 "parser.y"
    {
        cb_tree l;

        l = cb_build_locale_name ((yyvsp[(2) - (4)]), (yyvsp[(4) - (4)]));
        if (l != cb_error_node) {
                current_program->locale_list =
                        cb_list_add (current_program->locale_list, l);
        }
  ;}
    break;

  case 162:

/* Line 1455 of yacc.c  */
#line 1658 "parser.y"
    {
        unsigned char *s = CB_LITERAL ((yyvsp[(4) - (4)]))->data;

        if (CB_LITERAL ((yyvsp[(4) - (4)]))->size != 1) {
                cb_error_x ((yyvsp[(4) - (4)]), _("Invalid currency sign '%s'"), s);
        }
        switch (*s) {
        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':
        case 'a':
        case 'A':
        case 'b':
        case 'B':
        case 'c':
        case 'C':
        case 'd':
        case 'D':
        case 'e':
        case 'E':
        case 'n':
        case 'N':
        case 'p':
        case 'P':
        case 'r':
        case 'R':
        case 's':
        case 'S':
        case 'v':
        case 'V':
        case 'x':
        case 'X':
        case 'z':
        case 'Z':
        case '+':
        case '-':
        case ',':
        case '.':
        case '*':
        case '/':
        case ';':
        case '(':
        case ')':
        case '=':
        case '"':
        case ' ':
                cb_error_x ((yyvsp[(4) - (4)]), _("Invalid currency sign '%s'"), s);
                break;
        default:
                break;
        }
        current_program->currency_symbol = s[0];
  ;}
    break;

  case 163:

/* Line 1455 of yacc.c  */
#line 1725 "parser.y"
    {
        current_program->decimal_point = ',';
        current_program->numeric_separator = '.';
  ;}
    break;

  case 164:

/* Line 1455 of yacc.c  */
#line 1735 "parser.y"
    { current_program->cursor_pos = (yyvsp[(3) - (3)]); ;}
    break;

  case 165:

/* Line 1455 of yacc.c  */
#line 1742 "parser.y"
    { current_program->crt_status = (yyvsp[(4) - (4)]); ;}
    break;

  case 166:

/* Line 1455 of yacc.c  */
#line 1749 "parser.y"
    {  PENDING ("SCREEN CONTROL"); ;}
    break;

  case 167:

/* Line 1455 of yacc.c  */
#line 1755 "parser.y"
    {  PENDING ("EVENT STATUS"); ;}
    break;

  case 170:

/* Line 1455 of yacc.c  */
#line 1767 "parser.y"
    {
                                   if ( cb_flag_debug_parser ) fprintf(stderr, "[DEBUG PARSER] INPUT_OUTPUT SECTION\n");
                           ;}
    break;

  case 175:

/* Line 1455 of yacc.c  */
#line 1785 "parser.y"
    {
                                   if ( cb_flag_debug_parser ) fprintf(stderr, "[DEBUG PARSER] FILE_CONTROL\n");
                           ;}
    break;

  case 177:

/* Line 1455 of yacc.c  */
#line 1794 "parser.y"
    {
        organized_seen = 0;
        if ((yyvsp[(3) - (3)]) == cb_error_node) {
                if ( cb_flag_debug_parser ) fprintf(stderr, "[DEBUG PARSER] SELECT ERROR\n");
                YYERROR;
        }

        /* build new file */
        current_file = build_file ((yyvsp[(3) - (3)]));
        if ( cb_flag_debug_parser ) fprintf(stderr, "[DEBUG PARSER] SELECT %s\n", current_file->cname);
        current_file->optional = CB_INTEGER ((yyvsp[(2) - (3)]))->val;
        current_file->data_compress = cb_data_compress;
        current_file->ls_utf16 = cb_ls_utf16;
        current_file->ls_mfmode = cb_flag_line_seq_mf;
        current_file->ls_notrunc = cb_flag_line_seq_notrunc;
        current_file->ls_expandtab = cb_flag_line_seq_expand_tab;
        if (cb_flag_share_all_default) current_file->sharing = cb_int(COB_SHARE_WITH_ALL);

        /* register the file */
        current_program->file_list =
                cb_cons (CB_TREE (current_file), current_program->file_list);
        
  ;}
    break;

  case 178:

/* Line 1455 of yacc.c  */
#line 1818 "parser.y"
    {
        if ( cb_flag_debug_parser ) fprintf(stderr, "[DEBUG PARSER] SELECT %s Validate\n", current_file->cname);
        validate_file (current_file, (yyvsp[(3) - (6)]));
  ;}
    break;

  case 181:

/* Line 1455 of yacc.c  */
#line 1830 "parser.y"
    {if ( cb_flag_debug_parser ) fprintf(stderr, "[DEBUG PARSER] SELECT %s assign \n", current_file->cname);;}
    break;

  case 185:

/* Line 1455 of yacc.c  */
#line 1835 "parser.y"
    {if ( cb_flag_debug_parser ) fprintf(stderr, "[DEBUG PARSER] SELECT %s status \n", current_file->cname);;}
    break;

  case 195:

/* Line 1455 of yacc.c  */
#line 1852 "parser.y"
    {
        current_file->assign = cb_build_assignment_name (current_file, (yyvsp[(5) - (5)]));
  ;}
    break;

  case 196:

/* Line 1455 of yacc.c  */
#line 1856 "parser.y"
    {
        current_file->fileid_assign = 1;
  ;}
    break;

  case 197:

/* Line 1455 of yacc.c  */
#line 1860 "parser.y"
    {
        current_file->organization = COB_ORG_LINE_SEQUENTIAL; 
        current_file->rec_mode     = CB_REC_MODE_VARIABLE;
        current_file->is_printer   = 1;
        current_file->fileid_assign = 1;
  ;}
    break;

  case 200:

/* Line 1455 of yacc.c  */
#line 1870 "parser.y"
    { 
                  current_file->is_printer   = 1;
                  current_file->organization = COB_ORG_LINE_SEQUENTIAL; 
                  current_file->rec_mode     = CB_REC_MODE_VARIABLE;
                ;}
    break;

  case 202:

/* Line 1455 of yacc.c  */
#line 1879 "parser.y"
    {
        current_file->external_assign = cb_assign_clause == CB_ASSIGN_EXTERNAL;
        current_file->dynanic_assign  = cb_assign_clause == CB_ASSIGN_DYNAMIC;
  ;}
    break;

  case 203:

/* Line 1455 of yacc.c  */
#line 1884 "parser.y"
    {
        current_file->external_assign = 1;
  ;}
    break;

  case 204:

/* Line 1455 of yacc.c  */
#line 1888 "parser.y"
    {
        current_file->dynanic_assign = 1;
  ;}
    break;

  case 206:

/* Line 1455 of yacc.c  */
#line 1896 "parser.y"
    {
        const char      *s;

        s = "$#@DUMMY@#$";
        (yyval) = cb_build_alphanumeric_literal ((unsigned char *)s, strlen(s), 0);
  ;}
    break;

  case 209:

/* Line 1455 of yacc.c  */
#line 1913 "parser.y"
    { current_file->access_mode = COB_ACCESS_SEQUENTIAL; ;}
    break;

  case 210:

/* Line 1455 of yacc.c  */
#line 1914 "parser.y"
    { current_file->access_mode = COB_ACCESS_DYNAMIC; ;}
    break;

  case 211:

/* Line 1455 of yacc.c  */
#line 1915 "parser.y"
    { current_file->access_mode = COB_ACCESS_RANDOM; ;}
    break;

  case 212:

/* Line 1455 of yacc.c  */
#line 1923 "parser.y"
    {
        struct cb_alt_key *p;
        struct cb_alt_key *l;

        p = cobc_malloc (sizeof (struct cb_alt_key));
        p->key = (yyvsp[(5) - (6)]);
        p->duplicates = CB_INTEGER ((yyvsp[(6) - (6)]))->val;
        p->next = NULL;
        p->key_compress = cb_key_compress;

        /* add to the end of list */
        if (current_file->alt_key_list == NULL) {
                current_file->alt_key_list = p;
        } else {
                l = current_file->alt_key_list;
                for (; l->next; l = l->next);
                l->next = p;
        }
  ;}
    break;

  case 213:

/* Line 1455 of yacc.c  */
#line 1949 "parser.y"
    {
        PENDING ("COLLATING SEQUENCE");
  ;}
    break;

  case 214:

/* Line 1455 of yacc.c  */
#line 1959 "parser.y"
    {
    current_file->file_status = (yyvsp[(4) - (5)]);
    if ((yyvsp[(5) - (5)])) {
                PENDING ("2nd FILE STATUS");
        }
  ;}
    break;

  case 219:

/* Line 1455 of yacc.c  */
#line 1980 "parser.y"
    { current_file->lock_mode = COB_LOCK_MANUAL; ;}
    break;

  case 220:

/* Line 1455 of yacc.c  */
#line 1981 "parser.y"
    { current_file->lock_mode = COB_LOCK_AUTOMATIC; ;}
    break;

  case 221:

/* Line 1455 of yacc.c  */
#line 1982 "parser.y"
    { current_file->lock_mode = COB_LOCK_EXCLUSIVE; ;}
    break;

  case 224:

/* Line 1455 of yacc.c  */
#line 1988 "parser.y"
    {
    current_file->lock_mode |= COB_LOCK_MULTIPLE;
  ;}
    break;

  case 225:

/* Line 1455 of yacc.c  */
#line 1992 "parser.y"
    { 
      if (!cb_flag_ignore_with_rollback)
        current_file->with_rollback = 1;
      current_file->lock_mode |= COB_LOCK_MULTIPLE;
      
      if (current_file->organization != COB_ORG_INDEXED)
        PENDING ("WITH ROLLBACK"); 
    ;}
    break;

  case 228:

/* Line 1455 of yacc.c  */
#line 2009 "parser.y"
    { current_file->line_advancing_clause = 1;;}
    break;

  case 231:

/* Line 1455 of yacc.c  */
#line 2021 "parser.y"
    {
        if (organized_seen) {
                cb_error (_("Invalid or duplicate ORGANIZED clause"));
        } else {
                current_file->organization = COB_ORG_INDEXED;
                organized_seen = 1;
        }
  ;}
    break;

  case 232:

/* Line 1455 of yacc.c  */
#line 2031 "parser.y"
    {
        if (organized_seen) {
                cb_error (_("Invalid or duplicate ORGANIZED clause"));
        } else {
                current_file->organization = COB_ORG_SEQUENTIAL;
                organized_seen = 1;
        }
  ;}
    break;

  case 233:

/* Line 1455 of yacc.c  */
#line 2041 "parser.y"
    {
        if (organized_seen) {
                cb_error (_("Invalid or duplicate ORGANIZED clause"));
        } else {
                if (cb_flag_sequential_line)
                     current_file->organization = COB_ORG_LINE_SEQUENTIAL;
                else
                     current_file->organization = COB_ORG_SEQUENTIAL;
                organized_seen = 1;
        }
  ;}
    break;

  case 234:

/* Line 1455 of yacc.c  */
#line 2053 "parser.y"
    {
        if (organized_seen) {
                cb_error (_("Invalid or duplicate ORGANIZED clause"));
        } else {
            if(cb_flag_relative_mf)
                current_file->organization = COB_ORG_RELATIVE;
            else
                current_file->organization = COB_ORG_RELATIVE;
            organized_seen = 1;
        }
  ;}
    break;

  case 235:

/* Line 1455 of yacc.c  */
#line 2065 "parser.y"
    {
        if (organized_seen) {
                cb_error (_("Invalid or duplicate ORGANIZED clause"));
        } else {
                current_file->organization = COB_ORG_LINE_SEQUENTIAL;
                organized_seen = 1;
        }
  ;}
    break;

  case 236:

/* Line 1455 of yacc.c  */
#line 2080 "parser.y"
    {
        cb_verify (cb_padding_character_clause, "PADDING CHARACTER");
  ;}
    break;

  case 237:

/* Line 1455 of yacc.c  */
#line 2089 "parser.y"
    { /* ignored */ ;}
    break;

  case 238:

/* Line 1455 of yacc.c  */
#line 2097 "parser.y"
    {
        current_file->key = (yyvsp[(4) - (5)]);
        current_file->key_duplicates = CB_INTEGER ((yyvsp[(5) - (5)]))->val;
        current_file->prim_key_compress = cb_key_compress;
  ;}
    break;

  case 239:

/* Line 1455 of yacc.c  */
#line 2105 "parser.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 240:

/* Line 1455 of yacc.c  */
#line 2106 "parser.y"
    { 
                                                (yyval) = cb_build_object_list((yyvsp[(1) - (3)]) , (yyvsp[(3) - (3)]), CB_OBJECT_LIST_FIELD); 
                                             ;}
    break;

  case 241:

/* Line 1455 of yacc.c  */
#line 2109 "parser.y"
    { 
                                                   (yyval) = cb_build_object_list((yyvsp[(1) - (4)]) , (yyvsp[(4) - (4)]), CB_OBJECT_LIST_FIELD); 
                                             ;}
    break;

  case 242:

/* Line 1455 of yacc.c  */
#line 2117 "parser.y"
    { current_file->key = (yyvsp[(4) - (4)]); ;}
    break;

  case 243:

/* Line 1455 of yacc.c  */
#line 2124 "parser.y"
    { /* ignored */ ;}
    break;

  case 244:

/* Line 1455 of yacc.c  */
#line 2125 "parser.y"
    { /* ignored */ ;}
    break;

  case 245:

/* Line 1455 of yacc.c  */
#line 2132 "parser.y"
    { current_file->sharing = (yyvsp[(3) - (3)]); ;}
    break;

  case 246:

/* Line 1455 of yacc.c  */
#line 2136 "parser.y"
    { (yyval) = cb_int(COB_SHARE_WITH_ALL); ;}
    break;

  case 247:

/* Line 1455 of yacc.c  */
#line 2137 "parser.y"
    { (yyval) = cb_int(COB_SHARE_WITH_NO_OTHER); ;}
    break;

  case 248:

/* Line 1455 of yacc.c  */
#line 2138 "parser.y"
    { (yyval) = cb_int(COB_SHARE_WITH_READ); ;}
    break;

  case 251:

/* Line 1455 of yacc.c  */
#line 2153 "parser.y"
    {
                                   if ( cb_flag_debug_parser ) fprintf(stderr, "[DEBUG PARSER] I_O_CONTROL\n");
                           ;}
    break;

  case 259:

/* Line 1455 of yacc.c  */
#line 2178 "parser.y"
    {
        cb_tree l;

        switch (CB_INTEGER ((yyvsp[(2) - (5)]))->val) {
        case 0:
                /* SAME AREA */
                break;
        case 1:
                /* SAME RECORD */
                for (l = (yyvsp[(5) - (5)]); l; l = CB_CHAIN (l)) {
                        if (CB_VALUE (l) != cb_error_node) {
                                CB_FILE (cb_ref (CB_VALUE (l)))->same_clause = samearea;
                        }
                }
                samearea++;
                break;
        case 2:
                /* SAME SORT-MERGE */
                break;
        }
  ;}
    break;

  case 260:

/* Line 1455 of yacc.c  */
#line 2202 "parser.y"
    { (yyval) = cb_int0; ;}
    break;

  case 261:

/* Line 1455 of yacc.c  */
#line 2203 "parser.y"
    { (yyval) = cb_int1; ;}
    break;

  case 262:

/* Line 1455 of yacc.c  */
#line 2204 "parser.y"
    { (yyval) = cb_int2; ;}
    break;

  case 263:

/* Line 1455 of yacc.c  */
#line 2205 "parser.y"
    { (yyval) = cb_int2; ;}
    break;

  case 264:

/* Line 1455 of yacc.c  */
#line 2212 "parser.y"
    {
        cb_verify (cb_multiple_file_tape_clause, "MULTIPLE FILE TAPE");
  ;}
    break;

  case 267:

/* Line 1455 of yacc.c  */
#line 2223 "parser.y"
    { ;}
    break;

  case 270:

/* Line 1455 of yacc.c  */
#line 2234 "parser.y"
    {
        cb_warning (_("APPLY Clause ignored"));
  ;}
    break;

  case 271:

/* Line 1455 of yacc.c  */
#line 2240 "parser.y"
    {;}
    break;

  case 272:

/* Line 1455 of yacc.c  */
#line 2241 "parser.y"
    {;}
    break;

  case 273:

/* Line 1455 of yacc.c  */
#line 2242 "parser.y"
    {;}
    break;

  case 274:

/* Line 1455 of yacc.c  */
#line 2243 "parser.y"
    {;}
    break;

  case 276:

/* Line 1455 of yacc.c  */
#line 2253 "parser.y"
    {
        if ( cb_flag_debug_parser ) fprintf(stderr, "[DEBUG PARSER] Finalize Data Division\n");
        finalize_section_files (current_program->file_rec_list);
        cb_validate_section_fields(&(current_program->working_storage));
        cb_validate_section_fields(&(current_program->local_storage));
        cb_validate_section_fields(&(current_program->linkage_storage));
   ;}
    break;

  case 281:

/* Line 1455 of yacc.c  */
#line 2274 "parser.y"
    {if ( cb_flag_debug_parser ) fprintf(stderr, "[DEBUG PARSER] DATA DIVISION\n");;}
    break;

  case 282:

/* Line 1455 of yacc.c  */
#line 2276 "parser.y"
    {if ( cb_flag_debug_parser ) fprintf(stderr, "[DEBUG PARSER] FILE SECTION\n");;}
    break;

  case 283:

/* Line 1455 of yacc.c  */
#line 2278 "parser.y"
    {if ( cb_flag_debug_parser ) fprintf(stderr, "[DEBUG PARSER] WORKING STORAGE\n");;}
    break;

  case 284:

/* Line 1455 of yacc.c  */
#line 2280 "parser.y"
    {if ( cb_flag_debug_parser ) fprintf(stderr, "[DEBUG PARSER] LOCAL STORAGE\n");  ;}
    break;

  case 285:

/* Line 1455 of yacc.c  */
#line 2282 "parser.y"
    {if ( cb_flag_debug_parser ) fprintf(stderr, "[DEBUG PARSER] LINKAGE STORAGE\n");  ;}
    break;

  case 288:

/* Line 1455 of yacc.c  */
#line 2295 "parser.y"
    { 
        if ( cb_flag_debug_parser ) fprintf(stderr, "[DEBUG PARSER] FILE SECTION\n");
        current_storage = CB_STORAGE_FILE; 
        ;}
    break;

  case 289:

/* Line 1455 of yacc.c  */
#line 2301 "parser.y"
    {
    /*
        if (cb_relaxed_syntax_check) {
                cb_warning (_("FILE SECTION header missing - assumed"));
        } else {
                cb_error (_("FILE SECTION header missing"));
        }
     */
     current_file_type = (yyvsp[(1) - (1)]);
     current_storage = CB_STORAGE_FILE;
  ;}
    break;

  case 291:

/* Line 1455 of yacc.c  */
#line 2338 "parser.y"
    {
        if ((yyvsp[(2) - (2)]) && (yyvsp[(2) - (2)]) != cb_error_node) {
                current_program->file_rec_list = cb_list_add(current_program->file_rec_list,
                                                             cb_build_pair(CB_TREE(current_file), (yyvsp[(2) - (2)])));
        } else {
                cb_error (_("RECORD description missing or invalid"));
        }
  ;}
    break;

  case 292:

/* Line 1455 of yacc.c  */
#line 2350 "parser.y"
    { (yyval) = cb_int0; ;}
    break;

  case 293:

/* Line 1455 of yacc.c  */
#line 2351 "parser.y"
    { (yyval) = cb_int1; ;}
    break;

  case 294:

/* Line 1455 of yacc.c  */
#line 2361 "parser.y"
    {
        if ((yyvsp[(1) - (1)]) == cb_error_node) {
                YYERROR;
        }

        current_file = CB_FILE (cb_ref ((yyvsp[(1) - (1)])));
        if ( cb_flag_debug_parser ) fprintf(stderr, "[DEBUG PARSER]  FILE NAME %s\n", current_file->cname);
        if (current_file_type == cb_int1) {
                current_file->organization = COB_ORG_SORT;
        }
  ;}
    break;

  case 295:

/* Line 1455 of yacc.c  */
#line 2373 "parser.y"
    {
    /* Shut up bison */
    dummy_tree = (yyvsp[(2) - (4)]);
  ;}
    break;

  case 298:

/* Line 1455 of yacc.c  */
#line 2385 "parser.y"
    {
    if (0 && current_file->global) {
        cb_error (_("File cannot have both EXTERNAL and GLOBAL clauses"));
    }
    current_file->external = 1;
  ;}
    break;

  case 299:

/* Line 1455 of yacc.c  */
#line 2392 "parser.y"
    {
    if (0 && current_file->external) {
        cb_error (_("File cannot have both EXTERNAL and GLOBAL clauses"));
    }
    current_file->global = 1;
  ;}
    break;

  case 309:

/* Line 1455 of yacc.c  */
#line 2414 "parser.y"
    { /* ignored */ ;}
    break;

  case 313:

/* Line 1455 of yacc.c  */
#line 2453 "parser.y"
    {
        if (cb_flag_ls_ignore_rec_size && current_file->organization == COB_ORG_LINE_SEQUENTIAL) {
                cb_warning (_("RECORD clause ignored for LINE SEQUENTIAL"));
        } else {
                current_file->record_contain_clause = cb_flag_record_contains_strict;
                current_file->record_max = cb_get_int ((yyvsp[(3) - (4)]));
                if (current_file->record_max < 1)  {
                        current_file->record_max = 1;
                        cb_error (_("RECORD clause invalid"));
                }
                if (current_file->record_contain_clause)
                    current_file->record_min = current_file->record_max;
                if (current_file->rec_mode == CB_REC_MODE_DEFAULT)
                        current_file->rec_mode = CB_REC_MODE_FIX;
        }
  ;}
    break;

  case 314:

/* Line 1455 of yacc.c  */
#line 2470 "parser.y"
    {
        int     error_ind = 0;

        if (cb_flag_ls_ignore_rec_size && current_file->organization == COB_ORG_LINE_SEQUENTIAL) {
                cb_warning (_("RECORD clause ignored for LINE SEQUENTIAL"));
        } else {
                current_file->record_contain_clause = cb_flag_record_contains_strict;
                current_file->record_min = cb_get_int ((yyvsp[(3) - (6)]));
                current_file->record_max = cb_get_int ((yyvsp[(5) - (6)]));
                if (current_file->record_min < 0)  {
                        current_file->record_min = 0;
                        error_ind = 1;
                }
                if (current_file->record_max < 1)  {
                        current_file->record_max = 1;
                        error_ind = 1;
                }
                if (current_file->record_max < current_file->record_min)  {
                        error_ind = 1;
                }
                if (error_ind) {
                        cb_error (_("RECORD clause invalid"));
                }
                if (cb_flag_recmode_osvs && (current_file->rec_mode == CB_REC_MODE_DEFAULT))
                        current_file->rec_mode = CB_REC_MODE_VARIABLE;
        }
  ;}
    break;

  case 315:

/* Line 1455 of yacc.c  */
#line 2499 "parser.y"
    {
        int     error_ind = 0;

        if ((yyvsp[(6) - (9)]) && (yyvsp[(7) - (9)]) ) current_file->record_contain_clause = cb_flag_record_contains_strict;
        current_file->record_min = (yyvsp[(6) - (9)]) ? cb_get_int ((yyvsp[(6) - (9)])) : 0;
        current_file->record_max = (yyvsp[(7) - (9)]) ? cb_get_int ((yyvsp[(7) - (9)])) : 0;
        if (cb_flag_record_depending_iso && current_file->record_depending && current_file->record_min == 0) {
            current_file->record_min = 1;
        }
        if ((yyvsp[(6) - (9)]) && current_file->record_min < 0)  {
                current_file->record_min = 0;
                error_ind = 1;
        }
        if ((yyvsp[(7) - (9)]) && current_file->record_max < 1)  {
                current_file->record_max = 1;
                error_ind = 1;
        }
        if (((yyvsp[(6) - (9)]) || (yyvsp[(7) - (9)])) && current_file->record_max > 0  && current_file->record_max < current_file->record_min)  {
                error_ind = 1;
        }
        if ((current_file->rec_mode == CB_REC_MODE_DEFAULT) && 
            ((current_file->record_min != current_file->record_max) || (current_file->record_max == 0))){
                current_file->rec_mode = CB_REC_MODE_VARIABLE;
        } 
        if (error_ind) {
                cb_error (_("RECORD clause invalid"));
        }
  ;}
    break;

  case 317:

/* Line 1455 of yacc.c  */
#line 2531 "parser.y"
    {
        current_file->record_depending = (yyvsp[(3) - (3)]);
  ;}
    break;

  case 318:

/* Line 1455 of yacc.c  */
#line 2537 "parser.y"
    { (yyval) = NULL; ;}
    break;

  case 319:

/* Line 1455 of yacc.c  */
#line 2538 "parser.y"
    { (yyval) = (yyvsp[(2) - (2)]); ;}
    break;

  case 320:

/* Line 1455 of yacc.c  */
#line 2542 "parser.y"
    { (yyval) = NULL; ;}
    break;

  case 321:

/* Line 1455 of yacc.c  */
#line 2543 "parser.y"
    { (yyval) = (yyvsp[(2) - (2)]); ;}
    break;

  case 322:

/* Line 1455 of yacc.c  */
#line 2551 "parser.y"
    {
        cb_verify (cb_label_records_clause, "LABEL RECORDS");
  ;}
    break;

  case 327:

/* Line 1455 of yacc.c  */
#line 2569 "parser.y"
    {
        cb_verify (cb_value_of_clause, "VALUE OF");
  ;}
    break;

  case 328:

/* Line 1455 of yacc.c  */
#line 2573 "parser.y"
    {
        if (!current_file->assign || cb_value_id_priority) {
                current_file->external_assign = 0;
                if (current_file->assign 
                    && CB_TREE_TAG(current_file->assign) == CB_TAG_REFERENCE ) {
                    current_program->reference_list = cb_list_remove(current_program->reference_list, current_file->assign);
                }
                current_file->assign = cb_build_assignment_name (current_file, (yyvsp[(5) - (5)]));
        }
  ;}
    break;

  case 333:

/* Line 1455 of yacc.c  */
#line 2599 "parser.y"
    { cb_warn_undefine = 1;;}
    break;

  case 334:

/* Line 1455 of yacc.c  */
#line 2601 "parser.y"
    {
        cb_warn_undefine = 0;
        cb_verify (cb_data_records_clause, "DATA RECORDS");
  ;}
    break;

  case 335:

/* Line 1455 of yacc.c  */
#line 2613 "parser.y"
    {
        if (current_file->organization != COB_ORG_LINE_SEQUENTIAL && 
            current_file->organization != COB_ORG_SEQUENTIAL) {
                cb_error (_("LINAGE clause with wrong file type"));
                (yyval) = cb_error_node;
        } else {
                current_file->linage = (yyvsp[(3) - (5)]);                
                current_file->is_printer = 1;
                if (current_linage == 0) {
                        linage_file = current_file;
                }
                current_linage++;
        }
  ;}
    break;

  case 341:

/* Line 1455 of yacc.c  */
#line 2641 "parser.y"
    {
        current_file->latfoot = (yyvsp[(5) - (6)]);
  ;}
    break;

  case 342:

/* Line 1455 of yacc.c  */
#line 2648 "parser.y"
    {
        current_file->lattop = (yyvsp[(4) - (5)]);
  ;}
    break;

  case 343:

/* Line 1455 of yacc.c  */
#line 2655 "parser.y"
    {
        current_file->latbot = (yyvsp[(4) - (5)]);
  ;}
    break;

  case 344:

/* Line 1455 of yacc.c  */
#line 2665 "parser.y"
    { /*CIT*/ 
    if (current_file->rec_mode != CB_REC_MODE_DEFAULT) {
        cb_warning ( _("Multiple RECORDING clause ignored '%s'"), current_file->name);
    }
    if ((yyvsp[(4) - (4)]) != cb_error_node) {      
        if (CB_REFERENCE_P((yyvsp[(4) - (4)]))) {
            if ((strncasecmp (CB_NAME((yyvsp[(4) - (4)])), "V", 1) == 0) || (strncasecmp (CB_NAME((yyvsp[(4) - (4)])), "S", 1) == 0))
                if (current_file->organization == COB_ORG_LINE_SEQUENTIAL) {
                    if (cb_flag_line_seq_recording_mode) {
                        current_file->rec_mode = CB_REC_MODE_VARIABLE;  
                    }
                } else {
                    current_file->rec_mode = CB_REC_MODE_VARIABLE;
                }
            else if (strncasecmp (CB_NAME((yyvsp[(4) - (4)])), "F", 1) == 0) {
                if (current_file->organization == COB_ORG_LINE_SEQUENTIAL) {
                    if (cb_flag_line_seq_recording_mode) {
                        current_file->rec_mode = CB_REC_MODE_FIX;  
                    }
                } else {
                    current_file->rec_mode = CB_REC_MODE_FIX;  
                }
            }                              
            else if (strncasecmp (CB_NAME((yyvsp[(4) - (4)])), "U", 1) != 0)
                cb_warning ( _("Unsupported RECORDING mode '%s'"), CB_NAME((yyvsp[(4) - (4)])));
        }
    }
  ;}
    break;

  case 345:

/* Line 1455 of yacc.c  */
#line 2700 "parser.y"
    {
        if ((yyvsp[(3) - (3)]) != cb_error_node) {
                cb_tree x;

                x = cb_ref ((yyvsp[(3) - (3)]));
                if (!CB_ALPHABET_NAME_P (x)) {
                        cb_error_x ((yyvsp[(3) - (3)]), _("Alphabet-name is expected '%s'"), cb_name ((yyvsp[(3) - (3)])));
                } else if (CB_ALPHABET_NAME (x)->custom_list) {
                        PENDING ("CODE-SET");
                }
        }
  ;}
    break;

  case 346:

/* Line 1455 of yacc.c  */
#line 2717 "parser.y"
    {
    cb_warning (_("file descriptor REPORT IS"));
  ;}
    break;

  case 347:

/* Line 1455 of yacc.c  */
#line 2721 "parser.y"
    {
    cb_warning (_("file descriptor REPORTS ARE"));
  ;}
    break;

  case 348:

/* Line 1455 of yacc.c  */
#line 2732 "parser.y"
    { current_storage = CB_STORAGE_WORKING; 
                                  if ( cb_flag_debug_parser ) fprintf(stderr, "[DEBUG PARSER] WORKING-STORAGE\n");
                                ;}
    break;

  case 349:

/* Line 1455 of yacc.c  */
#line 2736 "parser.y"
    {
       
        if ((yyvsp[(5) - (6)])) {
                current_program->working_storage =
                        cb_field_add (current_program->working_storage, CB_FIELD ((yyvsp[(5) - (6)])));
        }
  ;}
    break;

  case 350:

/* Line 1455 of yacc.c  */
#line 2746 "parser.y"
    {
    /* cb_field_tree_check_typedef(current_field);*/
        current_field = NULL;
    description_field = NULL;
    cb_clear_real_field ();
  ;}
    break;

  case 351:

/* Line 1455 of yacc.c  */
#line 2753 "parser.y"
    {
    /*CIT*/
    /*Validation will be done later when all data section have been parsed*/
    (yyval) = CB_TREE (description_field);
  ;}
    break;

  case 355:

/* Line 1455 of yacc.c  */
#line 2777 "parser.y"
    {
        cb_tree x;

        x = cb_build_field_tree ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]), current_field, current_storage, current_file);
        if (x == cb_error_node) {
                YYERROR;
        } else {
            current_field = CB_FIELD (x);
        }
  ;}
    break;

  case 356:

/* Line 1455 of yacc.c  */
#line 2788 "parser.y"
    {
    if (current_field->flag_is_typedef) {
        /* move it out of the storage*/
        if ((current_field->level != 1) && (current_field->level != 77 )) {
            cb_error (_("is TYPEDEF must be Level 01 or 77"));            
        }
        if (!description_field) {
            description_field = current_field;
        }
    } else {
        if (!qualifier && (current_field->level == 88 ||
            current_field->level == 77 || current_field->level == 66 ||
            current_field->flag_item_78)) {
            if (current_field->level == 77){
                    cb_warning (_("Item Level 77 without a data name"));                    
            } else {
                    cb_error (_("Item requires a data name"));    
            }
        }
        if (current_field->level == 88) {
            cb_validate_88_item (current_field);
        }
        if (current_field->flag_item_78) {
            /* Reset to last non-78 item */
            current_field = cb_validate_78_item (current_field);
        }
        if (!description_field) {
            description_field = current_field;
        }
    }
  ;}
    break;

  case 361:

/* Line 1455 of yacc.c  */
#line 2830 "parser.y"
    {
    (yyval) = cb_build_filler ();
    qualifier = NULL;
    non_const_word = 0;
  ;}
    break;

  case 362:

/* Line 1455 of yacc.c  */
#line 2836 "parser.y"
    {
    (yyval) = cb_build_filler ();
    qualifier = NULL;
    non_const_word = 0;
  ;}
    break;

  case 363:

/* Line 1455 of yacc.c  */
#line 2842 "parser.y"
    {
    (yyval) = (yyvsp[(1) - (1)]);
    qualifier = (yyvsp[(1) - (1)]);
    non_const_word = 0;
  ;}
    break;

  case 364:

/* Line 1455 of yacc.c  */
#line 2851 "parser.y"
    {
    (yyval) = (yyvsp[(1) - (1)]);
    qualifier = (yyvsp[(1) - (1)]);
    non_const_word = 0;
  ;}
    break;

  case 366:

/* Line 1455 of yacc.c  */
#line 2860 "parser.y"
    {
    current_field->flag_is_global = 1;
    cb_error (_("CONSTANT with GLOBAL clause is not yet supported"));
  ;}
    break;

  case 367:

/* Line 1455 of yacc.c  */
#line 2867 "parser.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 368:

/* Line 1455 of yacc.c  */
#line 2868 "parser.y"
    { (yyval) = cb_build_const_length ((yyvsp[(3) - (3)])); ;}
    break;

  case 369:

/* Line 1455 of yacc.c  */
#line 2869 "parser.y"
    { (yyval) = cb_build_const_length ((yyvsp[(3) - (3)])); ;}
    break;

  case 370:

/* Line 1455 of yacc.c  */
#line 2874 "parser.y"
    {
    cb_tree x;
    int level;

    level = cb_get_level ((yyvsp[(1) - (6)]));
    if (level && level != 1) {
        cb_error (_("CONSTANT item not at 01 level"));
    }
    x = cb_build_constant ((yyvsp[(2) - (6)]), (yyvsp[(6) - (6)]));
    CB_FIELD (x)->flag_item_78 = 1;
    CB_FIELD (x)->level = 1;
    cb_needs_01 = 1;
    /* Ignore return value */
    cb_validate_78_item (CB_FIELD (x));
  ;}
    break;

  case 371:

/* Line 1455 of yacc.c  */
#line 2893 "parser.y"
    {
    /* required to check redefines */
    (yyval) = NULL;
  ;}
    break;

  case 372:

/* Line 1455 of yacc.c  */
#line 2899 "parser.y"
    {
    /* required to check redefines */
    (yyval) = cb_true;
  ;}
    break;

  case 388:

/* Line 1455 of yacc.c  */
#line 2926 "parser.y"
    { 
            current_field->flag_is_typedef =1;
            if (cb_flag_global_typedef)
                current_field->flag_is_typedef_global = 1;
        ;}
    break;

  case 389:

/* Line 1455 of yacc.c  */
#line 2936 "parser.y"
    {
        if ((yyvsp[(0) - (2)]) != NULL) {
                /* hack for MF compatibility */
                if (cb_relaxed_syntax_check) {
                        cb_warning_suggestion ((yyvsp[(2) - (2)]), _("REDEFINES clause should follow entry-name"));
                } else {
                        cb_error_x ((yyvsp[(2) - (2)]), _("REDEFINES clause must follow entry-name"));
                }
        }

        current_field->redefines = cb_resolve_redefines (current_field, (yyvsp[(2) - (2)]));
        if (current_field->redefines == NULL) {
                YYERROR;
        }
  ;}
    break;

  case 390:

/* Line 1455 of yacc.c  */
#line 2958 "parser.y"
    {
   if (current_storage != CB_STORAGE_WORKING) {
      cb_error (_("EXTERNAL not allowed here"));
    } else if (current_field->level != 1 && current_field->level != 77) {
      cb_error (_("EXTERNAL only allowed at 01/77 level"));
    } else if (!qualifier) {
      cb_error (_("EXTERNAL requires a data name"));
    /* CIT
    } else if (current_field->flag_is_global) {
        cb_error (_("GLOBAL and EXTERNAL are mutually exclusive"));
    */
    } else if (current_field->flag_item_based) {
      cb_error (_("BASED and EXTERNAL are mutually exclusive"));
    } else if (current_field->redefines) {
      cb_error (_("EXTERNAL and REDEFINES are mutually exclusive"));
    } else {
      current_field->flag_external = 1;
      has_external = 1;
    }
  ;}
    break;

  case 391:

/* Line 1455 of yacc.c  */
#line 2981 "parser.y"
    { current_field->ename = NULL; ;}
    break;

  case 392:

/* Line 1455 of yacc.c  */
#line 2983 "parser.y"
    {
        struct cb_field *x;

        x = CB_FIELD(cb_build_field (cb_build_reference ((char *)(CB_LITERAL ((yyvsp[(2) - (2)]))->data))));
        current_field->ename = x->name;
 ;}
    break;

  case 393:

/* Line 1455 of yacc.c  */
#line 2995 "parser.y"
    {
    if (current_field->level != 1 && current_field->level != 77) {
        cb_error (_("GLOBAL only allowed at 01/77 level"));
    } else if (!qualifier) {
        cb_error (_("GLOBAL requires a data name"));
    /* CIT
    } else if (current_field->flag_external) {
        cb_error (_("GLOBAL and EXTERNAL are mutually exclusive"));
    */
    } else if (current_storage == CB_STORAGE_LOCAL) {
        if (cb_flag_ignore_global_in_local)
            cb_warning(_("GLOBAL in LOCAL STORAGE is ignored"));
        else
            cb_error (_("GLOBAL not allowed here"));
    } else {
        current_field->flag_is_global = 1;
    }
  ;}
    break;

  case 394:

/* Line 1455 of yacc.c  */
#line 3020 "parser.y"
    { 
        if ((yyvsp[(1) - (1)]) != cb_error_node) {
            current_field->pic = CB_PICTURE ((yyvsp[(1) - (1)]));
            if ((current_field->pic->category == CB_CATEGORY_NATIONAL || 
                 current_field->pic->category == CB_CATEGORY_NATIONAL_EDITED ) &&
                !current_field->flag_usage_set) {
                current_field->usage = CB_USAGE_NATIONAL;
            }
        }
    ;}
    break;

  case 397:

/* Line 1455 of yacc.c  */
#line 3039 "parser.y"
    {
        struct cb_field *f = cb_type((yyvsp[(3) - (3)]));
        if (f && f->flag_is_typedef) {
            cob_enterprise_map_field_from_type (current_field, f);
        } else {
            cb_error (_("Invalid USAGE") );
        }
        
    ;}
    break;

  case 398:

/* Line 1455 of yacc.c  */
#line 3049 "parser.y"
    {
        struct cb_field *f = cb_field((yyvsp[(2) - (2)]));
        if (f ) {
            if (cb_flag_as400_like) {
                if (current_field->flag_is_typedef) {
                    cb_error_x (CB_TREE (f), _("LIKE may not define a TYPEDEF '%s'"),   f->name);
                } else {
                    current_field->like = f;
                }
            } else {
                cob_enterprise_map_field_from_type (current_field, f);
            }
        } else {
            cb_error (_("Invalid USAGE") );
        }
    ;}
    break;

  case 399:

/* Line 1455 of yacc.c  */
#line 3068 "parser.y"
    { current_field->usage = CB_USAGE_BINARY; current_field->flag_usage_set = 1;;}
    break;

  case 400:

/* Line 1455 of yacc.c  */
#line 3069 "parser.y"
    { current_field->usage = CB_USAGE_BIT;    current_field->flag_usage_set = 1;;}
    break;

  case 401:

/* Line 1455 of yacc.c  */
#line 3070 "parser.y"
    { current_field->usage = CB_USAGE_BINARY; current_field->flag_usage_set = 1;;}
    break;

  case 402:

/* Line 1455 of yacc.c  */
#line 3071 "parser.y"
    { current_field->usage = CB_USAGE_FLOAT;  current_field->flag_usage_set = 1;;}
    break;

  case 403:

/* Line 1455 of yacc.c  */
#line 3072 "parser.y"
    { current_field->usage = CB_USAGE_DOUBLE; current_field->flag_usage_set = 1;;}
    break;

  case 404:

/* Line 1455 of yacc.c  */
#line 3073 "parser.y"
    { current_field->usage = CB_USAGE_PACKED; current_field->flag_usage_set = 1;;}
    break;

  case 405:

/* Line 1455 of yacc.c  */
#line 3074 "parser.y"
    { current_field->usage = CB_USAGE_BINARY; current_field->flag_usage_set = 1;
                                  current_field->flag_binary_pack =  (cb_flag_pack_comp_4 ? 1 : 0);;}
    break;

  case 406:

/* Line 1455 of yacc.c  */
#line 3076 "parser.y"
    { current_field->usage = CB_USAGE_COMP_5; current_field->flag_usage_set = 1;;}
    break;

  case 407:

/* Line 1455 of yacc.c  */
#line 3077 "parser.y"
    { current_field->usage = CB_USAGE_COMP_6; current_field->flag_usage_set = 1;;}
    break;

  case 408:

/* Line 1455 of yacc.c  */
#line 3078 "parser.y"
    { current_field->usage = CB_USAGE_COMP_X; current_field->flag_usage_set = 1;;}
    break;

  case 409:

/* Line 1455 of yacc.c  */
#line 3079 "parser.y"
    { current_field->usage = CB_USAGE_DISPLAY;current_field->flag_usage_set = 1;;}
    break;

  case 410:

/* Line 1455 of yacc.c  */
#line 3080 "parser.y"
    { current_field->usage = CB_USAGE_INDEX;  current_field->flag_usage_set = 1;;}
    break;

  case 411:

/* Line 1455 of yacc.c  */
#line 3081 "parser.y"
    { current_field->usage = CB_USAGE_PACKED; current_field->flag_usage_set = 1;;}
    break;

  case 412:

/* Line 1455 of yacc.c  */
#line 3083 "parser.y"
    {
        current_field->usage = CB_USAGE_POINTER;
        current_field->flag_is_pointer = 1;
        current_field->flag_usage_set = 1;
  ;}
    break;

  case 413:

/* Line 1455 of yacc.c  */
#line 3089 "parser.y"
    {
        current_field->usage = CB_USAGE_PROGRAM_POINTER;
        current_field->flag_is_pointer = 1;
        current_field->flag_usage_set = 1;
  ;}
    break;

  case 414:

/* Line 1455 of yacc.c  */
#line 3094 "parser.y"
    { current_field->usage = CB_USAGE_SIGNED_SHORT; current_field->flag_usage_set = 1;;}
    break;

  case 415:

/* Line 1455 of yacc.c  */
#line 3095 "parser.y"
    { current_field->usage = CB_USAGE_SIGNED_INT;   current_field->flag_usage_set = 1;;}
    break;

  case 416:

/* Line 1455 of yacc.c  */
#line 3096 "parser.y"
    { current_field->usage = CB_USAGE_SIGNED_LONG;  current_field->flag_usage_set = 1;;}
    break;

  case 417:

/* Line 1455 of yacc.c  */
#line 3097 "parser.y"
    { current_field->usage = CB_USAGE_UNSIGNED_SHORT; current_field->flag_usage_set = 1;;}
    break;

  case 418:

/* Line 1455 of yacc.c  */
#line 3098 "parser.y"
    { current_field->usage = CB_USAGE_UNSIGNED_INT;   current_field->flag_usage_set = 1;;}
    break;

  case 419:

/* Line 1455 of yacc.c  */
#line 3099 "parser.y"
    { current_field->usage = CB_USAGE_UNSIGNED_LONG;  current_field->flag_usage_set = 1;;}
    break;

  case 420:

/* Line 1455 of yacc.c  */
#line 3100 "parser.y"
    { current_field->usage = CB_USAGE_SIGNED_CHAR;    current_field->flag_usage_set = 1;;}
    break;

  case 421:

/* Line 1455 of yacc.c  */
#line 3101 "parser.y"
    { current_field->usage = CB_USAGE_UNSIGNED_CHAR;  current_field->flag_usage_set = 1;;}
    break;

  case 422:

/* Line 1455 of yacc.c  */
#line 3102 "parser.y"
    { current_field->usage = CB_USAGE_SIGNED_CHAR;    current_field->flag_usage_set = 1;;}
    break;

  case 423:

/* Line 1455 of yacc.c  */
#line 3103 "parser.y"
    { current_field->usage = CB_USAGE_SIGNED_SHORT;   current_field->flag_usage_set = 1;;}
    break;

  case 424:

/* Line 1455 of yacc.c  */
#line 3104 "parser.y"
    { current_field->usage = CB_USAGE_UNSIGNED_SHORT; current_field->flag_usage_set = 1;;}
    break;

  case 425:

/* Line 1455 of yacc.c  */
#line 3105 "parser.y"
    { current_field->usage = CB_USAGE_SIGNED_SHORT;   current_field->flag_usage_set = 1;;}
    break;

  case 426:

/* Line 1455 of yacc.c  */
#line 3106 "parser.y"
    { current_field->usage = CB_USAGE_SIGNED_INT;     current_field->flag_usage_set = 1;;}
    break;

  case 427:

/* Line 1455 of yacc.c  */
#line 3107 "parser.y"
    { current_field->usage = CB_USAGE_UNSIGNED_INT;   current_field->flag_usage_set = 1;;}
    break;

  case 428:

/* Line 1455 of yacc.c  */
#line 3108 "parser.y"
    { current_field->usage = CB_USAGE_SIGNED_INT;     current_field->flag_usage_set = 1;;}
    break;

  case 429:

/* Line 1455 of yacc.c  */
#line 3109 "parser.y"
    { current_field->usage = CB_USAGE_SIGNED_LONG;    current_field->flag_usage_set = 1;;}
    break;

  case 430:

/* Line 1455 of yacc.c  */
#line 3110 "parser.y"
    { current_field->usage = CB_USAGE_UNSIGNED_LONG;  current_field->flag_usage_set = 1;;}
    break;

  case 431:

/* Line 1455 of yacc.c  */
#line 3111 "parser.y"
    { current_field->usage = CB_USAGE_SIGNED_LONG;    current_field->flag_usage_set = 1;;}
    break;

  case 432:

/* Line 1455 of yacc.c  */
#line 3113 "parser.y"
    {
        current_field->flag_usage_set = 1;
        if (sizeof(long) == 4) {
                current_field->usage = CB_USAGE_SIGNED_INT;
        } else {
                current_field->usage = CB_USAGE_SIGNED_LONG;
        }
  ;}
    break;

  case 433:

/* Line 1455 of yacc.c  */
#line 3122 "parser.y"
    {
        current_field->flag_usage_set = 1;
        if (sizeof(long) == 4) {
                current_field->usage = CB_USAGE_UNSIGNED_INT;
        } else {
                current_field->usage = CB_USAGE_UNSIGNED_LONG;
        }
  ;}
    break;

  case 434:

/* Line 1455 of yacc.c  */
#line 3131 "parser.y"
    {
        current_field->flag_usage_set = 1;
        if (sizeof(long) == 4) {
                current_field->usage = CB_USAGE_SIGNED_INT;
        } else {
                current_field->usage = CB_USAGE_SIGNED_LONG;
        }
  ;}
    break;

  case 435:

/* Line 1455 of yacc.c  */
#line 3140 "parser.y"
    { 
    current_field->usage = CB_USAGE_NATIONAL;
    current_field->flag_usage_set = 1;

  ;}
    break;

  case 436:

/* Line 1455 of yacc.c  */
#line 3146 "parser.y"
    { 
    current_field->usage = CB_USAGE_NATIONAL;
    current_field->flag_usage_set = 1;
  ;}
    break;

  case 437:

/* Line 1455 of yacc.c  */
#line 3157 "parser.y"
    {
        current_field->flag_sign_separate = CB_INTEGER ((yyvsp[(3) - (3)]))->val;
        current_field->flag_sign_leading  = 1;
  ;}
    break;

  case 438:

/* Line 1455 of yacc.c  */
#line 3162 "parser.y"
    {
        current_field->flag_sign_separate = CB_INTEGER ((yyvsp[(3) - (3)]))->val;
        current_field->flag_sign_leading  = 0;
  ;}
    break;

  case 439:

/* Line 1455 of yacc.c  */
#line 3174 "parser.y"
    {
       if (current_field->occurs_depending && !((yyvsp[(3) - (5)]))) {
            cb_verify (cb_odo_without_to, "ODO without TO clause");
       }
       current_field->occurs_min = (yyvsp[(3) - (5)]) ? cb_get_int ((yyvsp[(2) - (5)])) : 1;
       current_field->occurs_max = (yyvsp[(3) - (5)]) ? cb_get_int ((yyvsp[(3) - (5)])) : cb_get_int ((yyvsp[(2) - (5)]));
       current_field->indexes++;
       if (current_field->children){
           cb_increment_indexes(current_field->children);
       }
       if (current_field->indexes > COB_MAX_SUBSCRIPTS) {
           cb_error (_("Maximum OCCURS depth exceeded"));
       }
       current_field->flag_occurs = 1;
  ;}
    break;

  case 447:

/* Line 1455 of yacc.c  */
#line 3208 "parser.y"
    { (yyval) = NULL; ;}
    break;

  case 448:

/* Line 1455 of yacc.c  */
#line 3209 "parser.y"
    { (yyval) = (yyvsp[(2) - (2)]); ;}
    break;

  case 449:

/* Line 1455 of yacc.c  */
#line 3214 "parser.y"
    {
        current_field->occurs_depending = (yyvsp[(3) - (3)]);
  ;}
    break;

  case 450:

/* Line 1455 of yacc.c  */
#line 3221 "parser.y"
    {
        if ((yyvsp[(1) - (1)])) {
                cb_tree         l;
                struct cb_key   *keys;
                int             i;
                int             nkeys;

                l = (yyvsp[(1) - (1)]);
                nkeys = cb_list_length ((yyvsp[(1) - (1)]));
                keys = cobc_malloc (sizeof (struct cb_key) * nkeys);

                for (i = 0; i < nkeys; i++) {
                        keys[i].dir = CB_PURPOSE_INT (l);
                        keys[i].key = CB_VALUE (l);
                        l = CB_CHAIN (l);
                }
                current_field->keys = keys;
                current_field->nkeys = nkeys;
        }
  ;}
    break;

  case 451:

/* Line 1455 of yacc.c  */
#line 3245 "parser.y"
    {
        cb_tree l;

        for (l = (yyvsp[(4) - (4)]); l; l = CB_CHAIN (l)) {
                CB_PURPOSE (l) = (yyvsp[(1) - (4)]);
        if (qualifier && !CB_REFERENCE(CB_VALUE(l))->chain &&
                    strcasecmp(CB_NAME(CB_VALUE(l)), CB_NAME(qualifier))) {
                        CB_REFERENCE(CB_VALUE(l))->chain = qualifier;
                }
        }
        (yyval) = (yyvsp[(4) - (4)]);
  ;}
    break;

  case 452:

/* Line 1455 of yacc.c  */
#line 3260 "parser.y"
    {         (yyval) = cb_list_append (NULL, (yyvsp[(1) - (1)]));;}
    break;

  case 453:

/* Line 1455 of yacc.c  */
#line 3262 "parser.y"
    {         (yyval) = cb_list_append ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]));;}
    break;

  case 454:

/* Line 1455 of yacc.c  */
#line 3266 "parser.y"
    { (yyval) = cb_int (COB_ASCENDING); ;}
    break;

  case 455:

/* Line 1455 of yacc.c  */
#line 3267 "parser.y"
    { (yyval) = cb_int (COB_DESCENDING); ;}
    break;

  case 456:

/* Line 1455 of yacc.c  */
#line 3272 "parser.y"
    {
        current_field->index_list = (yyvsp[(3) - (3)]);
  ;}
    break;

  case 457:

/* Line 1455 of yacc.c  */
#line 3278 "parser.y"
    { (yyval) = cb_list_init ((yyvsp[(1) - (1)])); ;}
    break;

  case 458:

/* Line 1455 of yacc.c  */
#line 3280 "parser.y"
    { (yyval) = cb_list_add ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])); ;}
    break;

  case 459:

/* Line 1455 of yacc.c  */
#line 3285 "parser.y"
    {
        (yyval) = cb_build_index ((yyvsp[(1) - (1)]), cb_int1, 1, current_field, 0,CB_USAGE_INDEX);
  ;}
    break;

  case 460:

/* Line 1455 of yacc.c  */
#line 3294 "parser.y"
    { current_field->flag_justified = 1; ;}
    break;

  case 461:

/* Line 1455 of yacc.c  */
#line 3301 "parser.y"
    { current_field->flag_synchronized = 1; ;}
    break;

  case 465:

/* Line 1455 of yacc.c  */
#line 3313 "parser.y"
    { current_field->flag_blank_zero = 1; ;}
    break;

  case 466:

/* Line 1455 of yacc.c  */
#line 3321 "parser.y"
    {
    if (current_storage != CB_STORAGE_WORKING &&
        current_storage != CB_STORAGE_LINKAGE &&
        current_storage != CB_STORAGE_LOCAL) {
        cb_error (_("BASED not allowed here"));
    } else if (current_field->level != 1 && current_field->level != 77) {
        cb_error (_("BASED only allowed at 01/77 level"));
    } else if (!qualifier) {
        cb_error (_("BASED requires a data name"));
    } else if (current_field->flag_external) {
        cb_error (_("BASED and EXTERNAL are mutually exclusive"));
    } else if (current_field->redefines) {
        cb_error (_("BASED and REDEFINES are mutually exclusive"));
    } else if (current_field->flag_any_length) {
        cb_error (_("BASED and ANY LENGTH are mutually exclusive"));
    } else {
        current_field->flag_item_based = 1;
    }
  ;}
    break;

  case 467:

/* Line 1455 of yacc.c  */
#line 3345 "parser.y"
    { current_field->values = (yyvsp[(1) - (1)]); ;}
    break;

  case 468:

/* Line 1455 of yacc.c  */
#line 3347 "parser.y"
    { current_field->values = (yyvsp[(3) - (3)]); ;}
    break;

  case 470:

/* Line 1455 of yacc.c  */
#line 3352 "parser.y"
    { current_field->values = (yyvsp[(3) - (3)]); ;}
    break;

  case 472:

/* Line 1455 of yacc.c  */
#line 3357 "parser.y"
    { (yyval) = cb_list_init ((yyvsp[(1) - (1)])); ;}
    break;

  case 473:

/* Line 1455 of yacc.c  */
#line 3358 "parser.y"
    { (yyval) = cb_list_add ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])); ;}
    break;

  case 474:

/* Line 1455 of yacc.c  */
#line 3362 "parser.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 475:

/* Line 1455 of yacc.c  */
#line 3363 "parser.y"
    { (yyval) = cb_build_pair ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])); ;}
    break;

  case 476:

/* Line 1455 of yacc.c  */
#line 3364 "parser.y"
    { (yyval) = (yyvsp[(1) - (1)]); 
                                  if (!CB_CONST_P((yyval))) {
                                      cb_error_x((yyval),"is not a constant");
                                  }
                                ;}
    break;

  case 478:

/* Line 1455 of yacc.c  */
#line 3373 "parser.y"
    {
        if (current_field->level != 88) {
                cb_error (_("FALSE clause only allowed for 88 level"));
        }
    current_field->false_88 = cb_list_init ((yyvsp[(3) - (3)]));
  ;}
    break;

  case 479:

/* Line 1455 of yacc.c  */
#line 3386 "parser.y"
    {
        if (cb_ref ((yyvsp[(2) - (2)])) != cb_error_node) {
                if (CB_FIELD (cb_ref ((yyvsp[(2) - (2)])))->level == 01 ||
                    CB_FIELD (cb_ref ((yyvsp[(2) - (2)])))->level > 50) {
                        cb_error (_("RENAMES may not reference a level 01 or > 50"));
                } else {
                        current_field->redefines = CB_FIELD (cb_ref ((yyvsp[(2) - (2)])));
                        current_field->pic = current_field->redefines->pic;
                }
        }
  ;}
    break;

  case 480:

/* Line 1455 of yacc.c  */
#line 3398 "parser.y"
    {
        if (cb_ref ((yyvsp[(2) - (4)])) != cb_error_node && cb_ref ((yyvsp[(4) - (4)])) != cb_error_node) {
                if (CB_FIELD (cb_ref ((yyvsp[(2) - (4)])))->level == 01 ||
                    CB_FIELD (cb_ref ((yyvsp[(2) - (4)])))->level > 50) {
                        cb_error (_("RENAMES may not reference a level 01 or > 50"));
                } else if (CB_FIELD (cb_ref ((yyvsp[(4) - (4)])))->level == 01 ||
                    CB_FIELD (cb_ref ((yyvsp[(4) - (4)])))->level > 50) {
                        cb_error (_("RENAMES may not reference a level 01 or > 50"));
                } else {
                        current_field->redefines = CB_FIELD (cb_ref ((yyvsp[(2) - (4)])));
                        current_field->rename_thru = CB_FIELD (cb_ref ((yyvsp[(4) - (4)])));
                }
        }
  ;}
    break;

  case 481:

/* Line 1455 of yacc.c  */
#line 3418 "parser.y"
    {
    if (current_field->flag_item_based) {
        cb_error (_("BASED and ANY LENGTH are mutually exclusive"));
    } else {
        current_field->flag_any_length = 1;
  }
  ;}
    break;

  case 482:

/* Line 1455 of yacc.c  */
#line 3433 "parser.y"
    {
    current_storage = CB_STORAGE_LOCAL;
    if (current_program->nested_level) {
        cb_error (_("LOCAL-STORAGE not allowed in nested programs"));
    }
  ;}
    break;

  case 483:

/* Line 1455 of yacc.c  */
#line 3440 "parser.y"
    {
        if ((yyvsp[(5) - (5)])) {
                current_program->local_storage = 
                    cb_field_add (current_program->local_storage, CB_FIELD ((yyvsp[(5) - (5)])));
        }
  ;}
    break;

  case 484:

/* Line 1455 of yacc.c  */
#line 3454 "parser.y"
    { current_storage = CB_STORAGE_LINKAGE; ;}
    break;

  case 485:

/* Line 1455 of yacc.c  */
#line 3456 "parser.y"
    {
        if ((yyvsp[(5) - (5)])) {
                current_program->linkage_storage = 
                    cb_field_add (current_program->linkage_storage, CB_FIELD ((yyvsp[(5) - (5)])));        
                }
  ;}
    break;

  case 487:

/* Line 1455 of yacc.c  */
#line 3470 "parser.y"
    {
    cb_error (_("REPORT SECTION need preprocessing ... use SPCRW2"));
    current_storage = CB_STORAGE_REPORT;
  ;}
    break;

  case 494:

/* Line 1455 of yacc.c  */
#line 3503 "parser.y"
    {
    cb_warning (_("Report description using defaults"));
  ;}
    break;

  case 496:

/* Line 1455 of yacc.c  */
#line 3511 "parser.y"
    {
    cb_error (_("GLOBAL is not allowed with RD"));
  ;}
    break;

  case 535:

/* Line 1455 of yacc.c  */
#line 3582 "parser.y"
    { cb_warning (_("looking for Report line TYPE")); ;}
    break;

  case 589:

/* Line 1455 of yacc.c  */
#line 3690 "parser.y"
    { current_storage = CB_STORAGE_SCREEN; ;}
    break;

  case 590:

/* Line 1455 of yacc.c  */
#line 3691 "parser.y"
    {
        current_field = NULL;
    description_field = NULL;
    cb_clear_real_field ();
  ;}
    break;

  case 591:

/* Line 1455 of yacc.c  */
#line 3697 "parser.y"
    {
        struct cb_field *p;

        cb_expand_screen_field (description_field, NULL);
        for (p = description_field; p; p = p->sister) {
            cb_validate_field (p);
            }
        current_program->screen_storage = description_field;
        current_program->flag_screen = 1;
        ;}
    break;

  case 597:

/* Line 1455 of yacc.c  */
#line 3722 "parser.y"
    {
        cb_tree x;

        x = cb_build_field_tree ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]), current_field, current_storage, current_file);
        if (x == cb_error_node) {
                YYERROR;
        }

        current_field = CB_FIELD (x);
        if (current_field->parent) {
                int flg = current_field->parent->screen_flag;

                flg &= ~COB_SCREEN_BLANK_SCREEN;
                flg &= ~COB_SCREEN_BELL;
                current_field->screen_flag |= flg;
                current_field->screen_foreg = current_field->parent->screen_foreg;
                current_field->screen_backg = current_field->parent->screen_backg;
        } else {
            if (cb_flag_accept_with_auto)
                current_field->screen_flag |= COB_SCREEN_AUTO;
        }
  ;}
    break;

  case 598:

/* Line 1455 of yacc.c  */
#line 3745 "parser.y"
    {
    if (!qualifier && (current_field->level == 88 ||
        current_field->level == 77 || current_field->level == 66 ||
        current_field->flag_item_78)) {
        cb_error (_("Item requires a data name"));
    }
    if (current_field->level == 88) {
        cb_validate_88_item (current_field);
    }
    if (current_field->flag_item_78) {
        /* Reset to last non-78 item */
        current_field = cb_validate_78_item (current_field);
    }
    if (!description_field) {
        description_field = current_field;
    }
  ;}
    break;

  case 601:

/* Line 1455 of yacc.c  */
#line 3769 "parser.y"
    { current_field->screen_flag |= COB_SCREEN_BLANK_LINE; ;}
    break;

  case 602:

/* Line 1455 of yacc.c  */
#line 3770 "parser.y"
    { current_field->screen_flag |= COB_SCREEN_BLANK_SCREEN; ;}
    break;

  case 603:

/* Line 1455 of yacc.c  */
#line 3771 "parser.y"
    { current_field->screen_flag |= COB_SCREEN_BELL; ;}
    break;

  case 604:

/* Line 1455 of yacc.c  */
#line 3772 "parser.y"
    { current_field->screen_flag |= COB_SCREEN_NO_BELL; ;}
    break;

  case 605:

/* Line 1455 of yacc.c  */
#line 3773 "parser.y"
    { current_field->screen_flag |= COB_SCREEN_BLINK; ;}
    break;

  case 606:

/* Line 1455 of yacc.c  */
#line 3774 "parser.y"
    { current_field->screen_flag |= COB_SCREEN_ERASE_EOL; ;}
    break;

  case 607:

/* Line 1455 of yacc.c  */
#line 3775 "parser.y"
    { current_field->screen_flag |= COB_SCREEN_ERASE_EOS; ;}
    break;

  case 608:

/* Line 1455 of yacc.c  */
#line 3776 "parser.y"
    { current_field->screen_flag |= COB_SCREEN_HIGHLIGHT; ;}
    break;

  case 609:

/* Line 1455 of yacc.c  */
#line 3777 "parser.y"
    { current_field->screen_flag |= COB_SCREEN_LOWLIGHT; ;}
    break;

  case 610:

/* Line 1455 of yacc.c  */
#line 3778 "parser.y"
    { current_field->screen_flag |= COB_SCREEN_REVERSE; ;}
    break;

  case 611:

/* Line 1455 of yacc.c  */
#line 3779 "parser.y"
    { current_field->screen_flag |= COB_SCREEN_UNDERLINE; ;}
    break;

  case 612:

/* Line 1455 of yacc.c  */
#line 3780 "parser.y"
    { current_field->screen_flag |= COB_SCREEN_OVERLINE; ;}
    break;

  case 613:

/* Line 1455 of yacc.c  */
#line 3781 "parser.y"
    { current_field->screen_flag |= COB_SCREEN_AUTO; ;}
    break;

  case 614:

/* Line 1455 of yacc.c  */
#line 3782 "parser.y"
    { current_field->screen_flag &= ~COB_SCREEN_AUTO; ;}
    break;

  case 615:

/* Line 1455 of yacc.c  */
#line 3783 "parser.y"
    { current_field->screen_flag |= COB_SCREEN_SECURE; ;}
    break;

  case 616:

/* Line 1455 of yacc.c  */
#line 3784 "parser.y"
    { current_field->screen_flag |= COB_SCREEN_REQUIRED; ;}
    break;

  case 617:

/* Line 1455 of yacc.c  */
#line 3785 "parser.y"
    { current_field->screen_flag |= COB_SCREEN_FULL; ;}
    break;

  case 618:

/* Line 1455 of yacc.c  */
#line 3787 "parser.y"
    { current_field->screen_flag |= COB_SCREEN_UPPERCASE; ;}
    break;

  case 619:

/* Line 1455 of yacc.c  */
#line 3788 "parser.y"
    { current_field->screen_flag |= COB_SCREEN_LOWERCASE; ;}
    break;

  case 620:

/* Line 1455 of yacc.c  */
#line 3790 "parser.y"
    {

        current_field->screen_flag |= COB_SCREEN_PROMPT; 
        if((yyvsp[(4) - (4)])){
           unsigned char *s = CB_LITERAL ((yyvsp[(4) - (4)]))->data;
           if(!(yyvsp[(2) - (4)]) && !(yyvsp[(3) - (4)])){
               cb_error_x ((yyvsp[(4) - (4)]), _("Invalid PROMPT '%s'"), s);
           }
           if (CB_LITERAL ((yyvsp[(4) - (4)]))->size != 1) {
                   cb_error_x ((yyvsp[(4) - (4)]), _("Invalid PROMPT CHARACTER '%s'"), s);
           }
           current_field->screen_prompt = *s;
        }
        else{
            if((yyvsp[(2) - (4)]))
               if((yyvsp[(3) - (4)]))
                  cb_error (_("Invalid PROMPT CHARACTER IS"));
               else
                  cb_error (_("Invalid PROMPT CHARACTER"));
            else if((yyvsp[(3) - (4)]))
               cb_error (_("Invalid PROMPT IS"));
        }
  ;}
    break;

  case 621:

/* Line 1455 of yacc.c  */
#line 3814 "parser.y"
    { 
      if ((yyvsp[(5) - (5)]) == NULL) {
        current_field->screen_flag |= COB_SCREEN_LINE_PLUS;
        current_field->screen_line = cb_one;
      } else {
        if((yyvsp[(4) - (5)]) == NULL && CB_LITERAL_P((yyvsp[(5) - (5)]))){
            struct cb_literal *p =CB_LITERAL((yyvsp[(5) - (5)]));
            if (p->image[0] == '+' ) {
              current_field->screen_flag |= COB_SCREEN_LINE_PLUS;
            }
            else if( p->image[0] == '-'){
               p->sign = 1;
              current_field->screen_flag |= COB_SCREEN_LINE_MINUS;
            }
        }
        current_field->screen_line = (yyvsp[(5) - (5)]);
      }
  ;}
    break;

  case 622:

/* Line 1455 of yacc.c  */
#line 3833 "parser.y"
    {
      if ((yyvsp[(5) - (5)]) == NULL){
        current_field->screen_flag |= COB_SCREEN_COLUMN_PLUS;
        current_field->screen_column = cb_one;
      } else {
        if((yyvsp[(4) - (5)]) == NULL && CB_LITERAL_P((yyvsp[(5) - (5)]))){
            struct cb_literal *p = CB_LITERAL((yyvsp[(5) - (5)]));
            if(p->image[0] == '+'){
               current_field->screen_flag |= COB_SCREEN_COLUMN_PLUS;
            }
            else if (p->image[0] == '-'){
               p->sign = 1;
              current_field->screen_flag |= COB_SCREEN_COLUMN_MINUS;         
            }
        }
        current_field->screen_column = (yyvsp[(5) - (5)]);
      }
  ;}
    break;

  case 623:

/* Line 1455 of yacc.c  */
#line 3852 "parser.y"
    {
        current_field->screen_foreg = (yyvsp[(3) - (3)]);
  ;}
    break;

  case 624:

/* Line 1455 of yacc.c  */
#line 3856 "parser.y"
    {
        current_field->screen_backg = (yyvsp[(3) - (3)]);
  ;}
    break;

  case 632:

/* Line 1455 of yacc.c  */
#line 3867 "parser.y"
    {
        current_field->screen_from = (yyvsp[(2) - (2)]);
        current_field->screen_to = (yyvsp[(2) - (2)]);
        current_field->screen_flag |= COB_SCREEN_PROMPT;
        current_field->screen_flag |= COB_SCREEN_INPUT;
  ;}
    break;

  case 633:

/* Line 1455 of yacc.c  */
#line 3874 "parser.y"
    {
        current_field->screen_from = (yyvsp[(2) - (2)]);
  ;}
    break;

  case 634:

/* Line 1455 of yacc.c  */
#line 3878 "parser.y"
    {
        current_field->screen_to = (yyvsp[(2) - (2)]);
        current_field->screen_flag |= COB_SCREEN_PROMPT;
        current_field->screen_flag |= COB_SCREEN_INPUT;
  ;}
    break;

  case 635:

/* Line 1455 of yacc.c  */
#line 3884 "parser.y"
    {
    current_field->screen_control = (yyvsp[(3) - (3)]);

  ;}
    break;

  case 636:

/* Line 1455 of yacc.c  */
#line 3889 "parser.y"
    {
    current_field->screen_input_size = (yyvsp[(3) - (3)]);

  ;}
    break;

  case 637:

/* Line 1455 of yacc.c  */
#line 3897 "parser.y"
    {
        /* Nothing */
      (yyval) = NULL;
  ;}
    break;

  case 638:

/* Line 1455 of yacc.c  */
#line 3902 "parser.y"
    {
    current_field->screen_flag |= COB_SCREEN_LINE_PLUS;
  ;}
    break;

  case 639:

/* Line 1455 of yacc.c  */
#line 3906 "parser.y"
    {
        current_field->screen_flag |= COB_SCREEN_LINE_PLUS;
  ;}
    break;

  case 640:

/* Line 1455 of yacc.c  */
#line 3910 "parser.y"
    {
        current_field->screen_flag |= COB_SCREEN_LINE_MINUS;
  ;}
    break;

  case 641:

/* Line 1455 of yacc.c  */
#line 3914 "parser.y"
    {
    current_field->screen_flag |= COB_SCREEN_LINE_MINUS;
  ;}
    break;

  case 642:

/* Line 1455 of yacc.c  */
#line 3921 "parser.y"
    {
      (yyval) = NULL;
  ;}
    break;

  case 643:

/* Line 1455 of yacc.c  */
#line 3925 "parser.y"
    {
    current_field->screen_flag |= COB_SCREEN_COLUMN_PLUS;
  ;}
    break;

  case 644:

/* Line 1455 of yacc.c  */
#line 3929 "parser.y"
    {
        current_field->screen_flag |= COB_SCREEN_COLUMN_PLUS;
  ;}
    break;

  case 645:

/* Line 1455 of yacc.c  */
#line 3933 "parser.y"
    {
    current_field->screen_flag |= COB_SCREEN_COLUMN_MINUS;
  ;}
    break;

  case 646:

/* Line 1455 of yacc.c  */
#line 3937 "parser.y"
    {
        current_field->screen_flag |= COB_SCREEN_COLUMN_MINUS;
  ;}
    break;

  case 647:

/* Line 1455 of yacc.c  */
#line 3945 "parser.y"
    {
        current_field->occurs_max = cb_get_int ((yyvsp[(2) - (3)]));
    current_field->occurs_min = current_field->occurs_max;
        current_field->indexes++;
        current_field->flag_occurs = 1;
  ;}
    break;

  case 649:

/* Line 1455 of yacc.c  */
#line 3959 "parser.y"
    {
        current_section = NULL;
        current_paragraph = NULL;
        cb_define_system_name ("CONSOLE");
        cb_define_system_name ("SYSIN");
        cb_define_system_name ("SYSOUT");
        cb_define_system_name ("SYSERR");
        cb_set_in_procedure ();
  ;}
    break;

  case 650:

/* Line 1455 of yacc.c  */
#line 3969 "parser.y"
    {       
         
        current_program->gen_params_protection=cb_flag_protect_parameters;
        if (current_program->flag_main && !current_program->flag_chained && (yyvsp[(4) - (8)])) {
                cb_warning (_("Executable program requested but PROCEDURE/ENTRY has USING clause"));
                current_program->gen_params_protection=1;
        }
        emit_entry (current_program->program_id, 0, (yyvsp[(4) - (8)]), cb_merge_call_conv((yyvsp[(3) - (8)]), NULL), 1); /* main entry point */
        if (cb_flag_module_name_entry) {
            if ((current_program->next_program == NULL) && source_demangle_name && 
                (strcasecmp(source_demangle_name, current_program->program_id) != 0)) {                
                    emit_entry (source_demangle_name, 0, (yyvsp[(4) - (8)]), cb_merge_call_conv((yyvsp[(3) - (8)]), NULL), 1); /* Entry point using object name*/
            }
        }
        if (cb_initcall_list) {
            struct cb_text_list *l;

            BEGIN_STATEMENT_1("INITCALL",0,0);
            for (l=cb_initcall_list; l; l = l->next) {
               int lt = strlen(l->text) ;
               cb_emit_call(cb_build_alphanumeric_literal((unsigned char*)l->text, lt,0), 0,
                            NULL, NULL, NULL, NULL);
            }
        }
  ;}
    break;

  case 651:

/* Line 1455 of yacc.c  */
#line 3995 "parser.y"
    {
        if (current_paragraph) {
                if (current_paragraph->exit_label) {
                        emit_statement (current_paragraph->exit_label);
                }
                emit_statement (cb_build_perform_exit (current_paragraph));
        }
        if (current_section) {
                if (current_section->exit_label) {
                        emit_statement (current_section->exit_label);
                }
                emit_statement (cb_build_perform_exit (current_section));
        }
  ;}
    break;

  case 652:

/* Line 1455 of yacc.c  */
#line 4012 "parser.y"
    { (yyval) = NULL; ;}
    break;

  case 653:

/* Line 1455 of yacc.c  */
#line 4014 "parser.y"
    {
    call_mode = CB_CALL_BY_REFERENCE;
    size_mode = CB_SIZE_4;
  ;}
    break;

  case 654:

/* Line 1455 of yacc.c  */
#line 4019 "parser.y"
    { 
      (yyval) = cb_validate_procedure_param_list((yyvsp[(3) - (3)]));
  ;}
    break;

  case 655:

/* Line 1455 of yacc.c  */
#line 4023 "parser.y"
    {
    call_mode = CB_CALL_BY_REFERENCE;
    current_program->flag_chained = 1;
  ;}
    break;

  case 656:

/* Line 1455 of yacc.c  */
#line 4028 "parser.y"
    { 
        (yyval) = (yyvsp[(3) - (3)]); 

  ;}
    break;

  case 657:

/* Line 1455 of yacc.c  */
#line 4035 "parser.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 658:

/* Line 1455 of yacc.c  */
#line 4037 "parser.y"
    { (yyval) = cb_list_append ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])); ;}
    break;

  case 659:

/* Line 1455 of yacc.c  */
#line 4042 "parser.y"
    {
    (yyval) = cb_build_pair (cb_int (call_mode), cb_build_identifier ((yyvsp[(4) - (4)])));
    CB_SIZES ((yyval)) = size_mode;
  ;}
    break;

  case 661:

/* Line 1455 of yacc.c  */
#line 4051 "parser.y"
    {
    call_mode = CB_CALL_BY_REFERENCE;
  ;}
    break;

  case 662:

/* Line 1455 of yacc.c  */
#line 4055 "parser.y"
    {
    if (current_program->flag_chained) {
        cb_error (_("BY VALUE not allowed in CHAINED program"));
    } else {
        call_mode = CB_CALL_BY_VALUE;
    }
  ;}
    break;

  case 663:

/* Line 1455 of yacc.c  */
#line 4063 "parser.y"
    {
    call_mode = CB_CALL_BY_DESCRIPTOR;
  ;}
    break;

  case 664:

/* Line 1455 of yacc.c  */
#line 4070 "parser.y"
    {
        size_mode &= ~CB_SIZE_DELIMITED;
  ;}
    break;

  case 665:

/* Line 1455 of yacc.c  */
#line 4074 "parser.y"
    {
    if (!current_program->flag_is_external) {
        cb_error (_("DELIMITED only availaible for external ENTRY definition"));
    } else {
        size_mode |= CB_SIZE_DELIMITED;
    }
  ;}
    break;

  case 667:

/* Line 1455 of yacc.c  */
#line 4086 "parser.y"
    {
    if (CB_CALL_BY(call_mode) != CB_CALL_BY_VALUE) {
        cb_error (_("SIZE only allowed for BY VALUE items"));
    } else {
        size_mode = CB_SIZE_AUTO;
    }
  ;}
    break;

  case 668:

/* Line 1455 of yacc.c  */
#line 4094 "parser.y"
    {
    if (CB_CALL_BY(call_mode) != CB_CALL_BY_VALUE) {
        cb_error (_("SIZE only allowed for BY VALUE items"));
    } else {
        size_mode = CB_SIZE_4;
    }
  ;}
    break;

  case 669:

/* Line 1455 of yacc.c  */
#line 4102 "parser.y"
    {
    unsigned char *s = CB_LITERAL ((yyvsp[(4) - (4)]))->data;

    if (CB_CALL_BY(call_mode) != CB_CALL_BY_VALUE) {
        cb_error (_("SIZE only allowed for BY VALUE items"));
    } else if (CB_LITERAL ((yyvsp[(4) - (4)]))->size != 1) {
        cb_error_x ((yyvsp[(4) - (4)]), _("Invalid value for SIZE"));
    } else {
        size_mode = CB_SIZE_UNSIGNED;
        switch (*s) {
        case '1':
            size_mode |= CB_SIZE_1;
            break;
        case '2':
            size_mode |= CB_SIZE_2;
            break;
        case '4':
            size_mode |= CB_SIZE_4;
            break;
        case '8':
            size_mode |= CB_SIZE_8;
            break;
        default:
            cb_error_x ((yyvsp[(4) - (4)]), _("Invalid value for SIZE"));
            break;
        }
    }
  ;}
    break;

  case 670:

/* Line 1455 of yacc.c  */
#line 4131 "parser.y"
    {
    unsigned char *s = CB_LITERAL ((yyvsp[(3) - (3)]))->data;

    if (CB_CALL_BY(call_mode) != CB_CALL_BY_VALUE) {
        cb_error (_("SIZE only allowed for BY VALUE items"));
    } else if (CB_LITERAL ((yyvsp[(3) - (3)]))->size != 1) {
        cb_error_x ((yyvsp[(3) - (3)]), _("Invalid value for SIZE"));
    } else {
        size_mode = 0;
        switch (*s) {
        case '1':
            size_mode = CB_SIZE_1;
            break;
        case '2':
            size_mode = CB_SIZE_2;
            break;
        case '4':
            size_mode = CB_SIZE_4;
            break;
        case '8':
            size_mode = CB_SIZE_8;
            break;
        default:
            cb_error_x ((yyvsp[(3) - (3)]), _("Invalid value for SIZE"));
            break;
        }
    }
  ;}
    break;

  case 672:

/* Line 1455 of yacc.c  */
#line 4164 "parser.y"
    {
    if (CB_CALL_BY(call_mode) != CB_CALL_BY_REFERENCE && CB_CALL_BY(call_mode) != CB_CALL_BY_DEFAULT) {
        cb_error (_("OPTIONAL only allowed for BY REFERENCE items"));
    }
  ;}
    break;

  case 673:

/* Line 1455 of yacc.c  */
#line 4173 "parser.y"
    {
    if (current_program->prog_type == CB_FUNCTION_TYPE) {
        cb_error (_("RETURNING clause is required for a FUNCTION"));
    }
  ;}
    break;

  case 674:

/* Line 1455 of yacc.c  */
#line 4179 "parser.y"
    {
    if (cb_ref ((yyvsp[(2) - (2)])) != cb_error_node) {
        current_program->returning = (yyvsp[(2) - (2)]);
        if (cb_field ((yyvsp[(2) - (2)]))->storage != CB_STORAGE_LINKAGE) {
            cb_error (_("RETURNING item is not defined in LINKAGE SECTION"));
        }
    }
  ;}
    break;

  case 676:

/* Line 1455 of yacc.c  */
#line 4190 "parser.y"
    { in_declaratives = 1; ;}
    break;

  case 677:

/* Line 1455 of yacc.c  */
#line 4193 "parser.y"
    {
        in_declaratives = 0;
        if (current_paragraph) {
                if (current_paragraph->exit_label) {
                        emit_statement (current_paragraph->exit_label);
                }
                emit_statement (cb_build_perform_exit (current_paragraph));
                current_paragraph = NULL;
        }
        if (current_section) {
                if (current_section->exit_label) {
                        emit_statement (current_section->exit_label);
                }
                emit_statement (cb_build_perform_exit (current_section));
                current_section = NULL;
        }
  ;}
    break;

  case 684:

/* Line 1455 of yacc.c  */
#line 4228 "parser.y"
    {
        if (next_label_list) {
                cb_tree label;
                char name[16];

                sprintf (name, "L$%d", next_label_id);
                label = cb_build_reference (name);
                emit_statement (cb_build_label (label, NULL));
                current_program->label_list =
                        cb_list_append (current_program->label_list, next_label_list);
                next_label_list = NULL;
                next_label_id++;
        }
        if ((yyvsp[(2) - (2)]) == cb_int1)

    /* check_unreached = 0; */
  ;}
    break;

  case 685:

/* Line 1455 of yacc.c  */
#line 4246 "parser.y"
    {
    check_unreached = 0;
  ;}
    break;

  case 686:

/* Line 1455 of yacc.c  */
#line 4250 "parser.y"
    {
    /* check_unreached = 0; */
  ;}
    break;

  case 687:

/* Line 1455 of yacc.c  */
#line 4262 "parser.y"
    {
    non_const_word = 0;
    check_unreached = 0;
        if ((yyvsp[(1) - (4)]) == cb_error_node) {
                YYERROR;
        }
         
        if ((yyvsp[(1) - (4)])->source_column > 4) cb_check_feature_x ((yyvsp[(1) - (4)]), cb_syntax_ibm5_2, "SECTION name must be at COLUMN 8 to 11");
        /* Exit the last section */
        if (current_paragraph) {
                if (current_paragraph->exit_label) {
                        emit_statement (current_paragraph->exit_label);
                }
                emit_statement (cb_build_perform_exit (current_paragraph));
        }
        if (current_section) {
                if (current_section->exit_label) {
                        emit_statement (current_section->exit_label);
                }
                emit_statement (cb_build_perform_exit (current_section));
        }

        /* Begin a new section */
        current_section = CB_LABEL (cb_build_label ((yyvsp[(1) - (4)]), NULL));
        current_section->is_section = 1;
        current_paragraph = NULL;
        emit_statement (CB_TREE (current_section));
        current_program->all_label_list = cb_cons(CB_TREE(current_section),current_program->all_label_list);
  ;}
    break;

  case 688:

/* Line 1455 of yacc.c  */
#line 4295 "parser.y"
    {
        cb_tree label;

        non_const_word = 0;
        check_unreached = 0;
        (yyval) = cb_build_section_name ((yyvsp[(1) - (1)]), 1);
        /* if ($1 == cb_error_node) */
        if ((yyval) == cb_error_node) {
                YYERROR;
        }
        if ((yyvsp[(1) - (1)])->source_column > 4) cb_check_feature_x ((yyvsp[(1) - (1)]), cb_syntax_ibm5_2, "PARAGRAPH name must be at COLUMN 8 to 11");

        /* Exit the last paragraph */
        if (current_paragraph) {
                if (current_paragraph->exit_label) {
                        emit_statement (current_paragraph->exit_label);
                }
                emit_statement (cb_build_perform_exit (current_paragraph));
        }

        /* Begin a new paragraph */
        if (!current_section) {
                label = cb_build_reference ("MAIN SECTION");
                current_section = CB_LABEL (cb_build_label (label, NULL));
                current_section->is_section = 1;
                emit_statement (CB_TREE (current_section));
        }
        current_paragraph = CB_LABEL (cb_build_label ((yyval), current_section));
        if (current_section) {
                current_section->children =
                        cb_cons (CB_TREE (current_paragraph), current_section->children);
        }
        emit_statement (CB_TREE (current_paragraph));
        current_program->all_label_list = cb_cons(CB_TREE (current_paragraph),current_program->all_label_list);
  ;}
    break;

  case 690:

/* Line 1455 of yacc.c  */
#line 4338 "parser.y"
    {
    non_const_word = 0;
    check_unreached = 0;
    (yyval) = cb_build_section_name ((yyvsp[(1) - (1)]), 0);
        if ((yyval) != cb_error_node) {
                cb_error_x ((yyvsp[(1) - (1)]), _("Unknown statement '%s'"), CB_NAME ((yyvsp[(1) - (1)])));
        }
        YYERROR;
  ;}
    break;

  case 691:

/* Line 1455 of yacc.c  */
#line 4350 "parser.y"
    { (yyval) = cb_build_section_name ((yyvsp[(1) - (1)]), 0); ;}
    break;

  case 692:

/* Line 1455 of yacc.c  */
#line 4351 "parser.y"
    { (yyval) = cb_build_section_name ((yyvsp[(2) - (2)]), 0); ;}
    break;

  case 694:

/* Line 1455 of yacc.c  */
#line 4355 "parser.y"
    { /* ignore */ ;}
    break;

  case 695:

/* Line 1455 of yacc.c  */
#line 4364 "parser.y"
    {
        (yyval) = current_program->exec_list;
        current_program->exec_list = NULL;
  ;}
    break;

  case 696:

/* Line 1455 of yacc.c  */
#line 4368 "parser.y"
    {
        (yyval) = CB_TREE (current_statement);
        current_statement = NULL;
  ;}
    break;

  case 697:

/* Line 1455 of yacc.c  */
#line 4373 "parser.y"
    {
        (yyval) = cb_list_reverse (current_program->exec_list);
        current_program->exec_list = (yyvsp[(1) - (3)]);
        current_statement = CB_STATEMENT ((yyvsp[(2) - (3)]));
  ;}
    break;

  case 698:

/* Line 1455 of yacc.c  */
#line 4381 "parser.y"
    { (yyval)=cb_list_init(cb_build_continue ()); ;}
    break;

  case 700:

/* Line 1455 of yacc.c  */
#line 4386 "parser.y"
    {
        cb_tree label;

        if (!current_section) {
                label = cb_build_reference ("MAIN SECTION");
                current_section = CB_LABEL (cb_build_label (label, NULL));
                current_section->is_section = 1;
                emit_statement (CB_TREE (current_section));
        }
        if (!current_paragraph) {
                label = cb_build_reference ("MAIN PARAGRAPH");
                current_paragraph = CB_LABEL (cb_build_label (label, NULL));
                emit_statement (CB_TREE (current_paragraph));
                current_section->children =
                        cb_cons (CB_TREE (current_paragraph), current_section->children);
        }
  ;}
    break;

  case 758:

/* Line 1455 of yacc.c  */
#line 4464 "parser.y"
    {
        if (cb_verify (cb_next_sentence_phrase, "NEXT SENTENCE")) {
                cb_tree label;
                char    name[16];

                BEGIN_STATEMENT ("NEXT SENTENCE", 0);
                sprintf (name, "L$%d", next_label_id);
                label = cb_build_reference (name);
                next_label_list = cb_list_add (next_label_list, label);
                emit_statement (cb_build_goto (label, NULL, NULL));
        }
    check_unreached = 0;
  ;}
    break;

  case 759:

/* Line 1455 of yacc.c  */
#line 4486 "parser.y"
    {
        BEGIN_STATEMENT ("ACCEPT", TERM_ACCEPT);
        dispattrs = 0;
        if (cb_flag_accept_with_update)
            dispattrs |= COB_SCREEN_UPDATE;
        if (cb_flag_accept_with_auto)
            dispattrs |= COB_SCREEN_AUTO;
        dispprompt = 0;
        display_fgc = NULL;
        display_bgc = NULL;
        display_line = NULL;
        display_column = NULL;
        scroll = NULL;
        acc_exception_field = NULL;
        acc_timeout = NULL;
  ;}
    break;

  case 761:

/* Line 1455 of yacc.c  */
#line 4508 "parser.y"
    {
    cb_emit_accept ((yyvsp[(1) - (3)]), display_line, display_column, display_fgc, display_bgc, scroll, dispattrs, dispprompt, acc_exception_field, acc_timeout);
  ;}
    break;

  case 762:

/* Line 1455 of yacc.c  */
#line 4512 "parser.y"
    {
    cb_emit_accept_escape((yyvsp[(1) - (4)]));
  ;}
    break;

  case 763:

/* Line 1455 of yacc.c  */
#line 4516 "parser.y"
    {
    cb_emit_accept_line_or_col ((yyvsp[(1) - (3)]), 0);
  ;}
    break;

  case 764:

/* Line 1455 of yacc.c  */
#line 4520 "parser.y"
    {
    cb_emit_accept_line_or_col ((yyvsp[(1) - (3)]), 1);
  ;}
    break;

  case 765:

/* Line 1455 of yacc.c  */
#line 4524 "parser.y"
    {
    cb_emit_accept_date ((yyvsp[(1) - (3)]));
  ;}
    break;

  case 766:

/* Line 1455 of yacc.c  */
#line 4528 "parser.y"
    {
    cb_emit_accept_date_yyyymmdd ((yyvsp[(1) - (4)]));
  ;}
    break;

  case 767:

/* Line 1455 of yacc.c  */
#line 4532 "parser.y"
    {
    cb_emit_accept_date_yyyymmdd ((yyvsp[(1) - (3)]));
  ;}
    break;

  case 768:

/* Line 1455 of yacc.c  */
#line 4536 "parser.y"
    {
    cb_emit_accept_day ((yyvsp[(1) - (3)]));
  ;}
    break;

  case 769:

/* Line 1455 of yacc.c  */
#line 4540 "parser.y"
    {
    cb_emit_accept_day_yyyyddd ((yyvsp[(1) - (4)]));
  ;}
    break;

  case 770:

/* Line 1455 of yacc.c  */
#line 4544 "parser.y"
    {
    cb_emit_accept_day_yyyyddd ((yyvsp[(1) - (3)]));
  ;}
    break;

  case 771:

/* Line 1455 of yacc.c  */
#line 4548 "parser.y"
    {
    cb_emit_accept_day_of_week ((yyvsp[(1) - (3)]));
  ;}
    break;

  case 772:

/* Line 1455 of yacc.c  */
#line 4552 "parser.y"
    {
    cb_emit_accept_time ((yyvsp[(1) - (3)]));
  ;}
    break;

  case 773:

/* Line 1455 of yacc.c  */
#line 4556 "parser.y"
    {
    cb_emit_accept_command_line ((yyvsp[(1) - (3)]));
  ;}
    break;

  case 774:

/* Line 1455 of yacc.c  */
#line 4560 "parser.y"
    {
    cb_emit_accept_environment ((yyvsp[(1) - (4)]));
  ;}
    break;

  case 775:

/* Line 1455 of yacc.c  */
#line 4564 "parser.y"
    { 
        cb_emit_get_environment ((yyvsp[(4) - (5)]), (yyvsp[(1) - (5)]));
  ;}
    break;

  case 776:

/* Line 1455 of yacc.c  */
#line 4568 "parser.y"
    {
    cb_emit_accept_arg_number ((yyvsp[(1) - (3)]));
  ;}
    break;

  case 777:

/* Line 1455 of yacc.c  */
#line 4572 "parser.y"
    {
    cb_emit_accept_arg_value ((yyvsp[(1) - (4)]));
  ;}
    break;

  case 778:

/* Line 1455 of yacc.c  */
#line 4576 "parser.y"
    {
    cb_emit_accept_mnemonic ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
  ;}
    break;

  case 779:

/* Line 1455 of yacc.c  */
#line 4580 "parser.y"
    {
    cb_emit_accept_name ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
  ;}
    break;

  case 780:

/* Line 1455 of yacc.c  */
#line 4587 "parser.y"
    { display_line = (yyvsp[(2) - (2)]); ;}
    break;

  case 781:

/* Line 1455 of yacc.c  */
#line 4588 "parser.y"
    { display_column = (yyvsp[(2) - (2)]); ;}
    break;

  case 782:

/* Line 1455 of yacc.c  */
#line 4589 "parser.y"
    { display_line = (yyvsp[(2) - (2)]); display_column = cb_low;;}
    break;

  case 783:

/* Line 1455 of yacc.c  */
#line 4594 "parser.y"
    { (yyval) = (yyvsp[(3) - (4)]); ;}
    break;

  case 784:

/* Line 1455 of yacc.c  */
#line 4599 "parser.y"
    { (yyval) = (yyvsp[(3) - (4)]); ;}
    break;

  case 785:

/* Line 1455 of yacc.c  */
#line 4600 "parser.y"
    { (yyval) = (yyvsp[(3) - (4)]); ;}
    break;

  case 791:

/* Line 1455 of yacc.c  */
#line 4615 "parser.y"
    { PENDING("CONVERT");;}
    break;

  case 792:

/* Line 1455 of yacc.c  */
#line 4616 "parser.y"
    { PENDING("ECHO") ;;}
    break;

  case 793:

/* Line 1455 of yacc.c  */
#line 4617 "parser.y"
    { dispattrs |= COB_SCREEN_BELL; ;}
    break;

  case 794:

/* Line 1455 of yacc.c  */
#line 4618 "parser.y"
    { dispattrs |= COB_SCREEN_NO_BELL; ;}
    break;

  case 795:

/* Line 1455 of yacc.c  */
#line 4619 "parser.y"
    { dispattrs |= COB_SCREEN_BLINK; ;}
    break;

  case 796:

/* Line 1455 of yacc.c  */
#line 4620 "parser.y"
    { dispattrs |= COB_SCREEN_ERASE_EOL; ;}
    break;

  case 797:

/* Line 1455 of yacc.c  */
#line 4621 "parser.y"
    { dispattrs |= COB_SCREEN_ERASE_EOS; ;}
    break;

  case 798:

/* Line 1455 of yacc.c  */
#line 4622 "parser.y"
    { dispattrs |= COB_SCREEN_HIGHLIGHT; ;}
    break;

  case 799:

/* Line 1455 of yacc.c  */
#line 4623 "parser.y"
    { dispattrs |= COB_SCREEN_LOWLIGHT; ;}
    break;

  case 800:

/* Line 1455 of yacc.c  */
#line 4624 "parser.y"
    { dispattrs |= COB_SCREEN_REVERSE; ;}
    break;

  case 801:

/* Line 1455 of yacc.c  */
#line 4625 "parser.y"
    { dispattrs |= COB_SCREEN_UNDERLINE; ;}
    break;

  case 802:

/* Line 1455 of yacc.c  */
#line 4626 "parser.y"
    { dispattrs |= COB_SCREEN_OVERLINE; ;}
    break;

  case 803:

/* Line 1455 of yacc.c  */
#line 4627 "parser.y"
    { dispattrs |= COB_SCREEN_UPPERCASE; ;}
    break;

  case 804:

/* Line 1455 of yacc.c  */
#line 4628 "parser.y"
    { dispattrs |= COB_SCREEN_LOWERCASE; ;}
    break;

  case 805:

/* Line 1455 of yacc.c  */
#line 4630 "parser.y"
    {
        display_fgc = (yyvsp[(4) - (4)]);
  ;}
    break;

  case 806:

/* Line 1455 of yacc.c  */
#line 4634 "parser.y"
    {
        display_bgc = (yyvsp[(4) - (4)]);
  ;}
    break;

  case 807:

/* Line 1455 of yacc.c  */
#line 4638 "parser.y"
    {
    scroll = (yyvsp[(4) - (4)]);
  ;}
    break;

  case 808:

/* Line 1455 of yacc.c  */
#line 4642 "parser.y"
    {
    dispattrs |= COB_SCREEN_SCROLL_DOWN;
    scroll = (yyvsp[(4) - (4)]);
  ;}
    break;

  case 809:

/* Line 1455 of yacc.c  */
#line 4646 "parser.y"
    { dispattrs |= COB_SCREEN_AUTO; ;}
    break;

  case 810:

/* Line 1455 of yacc.c  */
#line 4647 "parser.y"
    { dispattrs &= ~COB_SCREEN_AUTO; ;}
    break;

  case 811:

/* Line 1455 of yacc.c  */
#line 4648 "parser.y"
    { dispattrs |= COB_SCREEN_FULL; ;}
    break;

  case 812:

/* Line 1455 of yacc.c  */
#line 4649 "parser.y"
    { dispattrs |= COB_SCREEN_REQUIRED; ;}
    break;

  case 813:

/* Line 1455 of yacc.c  */
#line 4650 "parser.y"
    { dispattrs |= COB_SCREEN_SECURE; ;}
    break;

  case 814:

/* Line 1455 of yacc.c  */
#line 4651 "parser.y"
    { dispattrs |= COB_SCREEN_UPDATE; ;}
    break;

  case 815:

/* Line 1455 of yacc.c  */
#line 4652 "parser.y"
    { dispattrs |= COB_SCREEN_PROMPT; ;}
    break;

  case 816:

/* Line 1455 of yacc.c  */
#line 4654 "parser.y"
    {
        unsigned char *s = CB_LITERAL ((yyvsp[(5) - (5)]))->data;

        if (CB_LITERAL ((yyvsp[(5) - (5)]))->size != 1) {
                cb_error_x ((yyvsp[(5) - (5)]), _("Invalid PROMPT CHARACTER '%s'"), s);
        }
        dispattrs |= COB_SCREEN_PROMPT; 
        dispprompt = *s;
  ;}
    break;

  case 817:

/* Line 1455 of yacc.c  */
#line 4663 "parser.y"
    {acc_timeout = (yyvsp[(3) - (3)]);;}
    break;

  case 818:

/* Line 1455 of yacc.c  */
#line 4667 "parser.y"
    { terminator_warning (TERM_ACCEPT); ;}
    break;

  case 819:

/* Line 1455 of yacc.c  */
#line 4668 "parser.y"
    { terminator_clear (TERM_ACCEPT); ;}
    break;

  case 820:

/* Line 1455 of yacc.c  */
#line 4677 "parser.y"
    { BEGIN_STATEMENT ("ADD", TERM_ADD);rounded_used = 0; ;}
    break;

  case 822:

/* Line 1455 of yacc.c  */
#line 4684 "parser.y"
    {
   /*CIT*/          
        if (CB_EXCEPTION_ENABLE (COB_EC_SIZE_OVERFLOW) ) {
            current_statement->handler_id = COB_EC_SIZE_OVERFLOW;
        } 
     
        if (current_statement->handler1 || current_statement->handler2) {
            cb_check_optimized_field((yyvsp[(1) - (4)]), 0);
            cb_check_optimized_field((yyvsp[(3) - (4)]), 0);
        } else {
            cb_check_optimized_field((yyvsp[(3) - (4)]), 1);
        }
        cb_emit_arithmetic ((yyvsp[(3) - (4)]), '+', cb_build_binary_list ((yyvsp[(1) - (4)]), '+'), !rounded_used);
  ;}
    break;

  case 823:

/* Line 1455 of yacc.c  */
#line 4699 "parser.y"
    {
   /*CIT*/
        if (CB_EXCEPTION_ENABLE (COB_EC_SIZE_OVERFLOW) ) {
            current_statement->handler_id = COB_EC_SIZE_OVERFLOW;
        } 
        if (current_statement->handler1 || current_statement->handler2) {
            cb_check_optimized_field((yyvsp[(1) - (5)]), 0);
            cb_check_optimized_field((yyvsp[(4) - (5)]), 0);
        } else {
            cb_check_optimized_field((yyvsp[(4) - (5)]), 1);
        }
        cb_emit_arithmetic ((yyvsp[(4) - (5)]), 0, cb_build_binary_list ((yyvsp[(1) - (5)]), '+'), !rounded_used);
  ;}
    break;

  case 824:

/* Line 1455 of yacc.c  */
#line 4713 "parser.y"
    {
        void * a2, *a4;
        a2 = (yyvsp[(2) - (6)]);
        a4 = (yyvsp[(4) - (6)]);
        if (CB_EXCEPTION_ENABLE (COB_EC_SIZE_OVERFLOW) ) {
            current_statement->handler_id = COB_EC_SIZE_OVERFLOW;
        } 
        if (current_statement->handler1 || current_statement->handler2 || (yyvsp[(5) - (6)]) != cb_int0) {
            a2 = cb_check_optimized_field_1((yyvsp[(2) - (6)]));
            a4 = cb_check_optimized_field_1((yyvsp[(4) - (6)]));
        }
        cb_emit_corresponding (cb_build_add, a4, a2, (yyvsp[(5) - (6)]));
  ;}
    break;

  case 826:

/* Line 1455 of yacc.c  */
#line 4729 "parser.y"
    { cb_list_add ((yyvsp[(0) - (2)]), (yyvsp[(2) - (2)])); ;}
    break;

  case 827:

/* Line 1455 of yacc.c  */
#line 4733 "parser.y"
    { terminator_warning (TERM_ADD); ;}
    break;

  case 828:

/* Line 1455 of yacc.c  */
#line 4734 "parser.y"
    { terminator_clear (TERM_ADD); ;}
    break;

  case 829:

/* Line 1455 of yacc.c  */
#line 4743 "parser.y"
    { BEGIN_STATEMENT ("ALLOCATE", 0); ;}
    break;

  case 831:

/* Line 1455 of yacc.c  */
#line 4749 "parser.y"
    {
        cb_emit_allocate ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]), NULL, (yyvsp[(2) - (3)]));
  ;}
    break;

  case 832:

/* Line 1455 of yacc.c  */
#line 4753 "parser.y"
    {
        cb_emit_allocate (NULL, (yyvsp[(5) - (5)]), (yyvsp[(1) - (5)]), (yyvsp[(3) - (5)]));
  ;}
    break;

  case 833:

/* Line 1455 of yacc.c  */
#line 4759 "parser.y"
    { (yyval) = NULL; ;}
    break;

  case 834:

/* Line 1455 of yacc.c  */
#line 4760 "parser.y"
    { (yyval) = (yyvsp[(2) - (2)]); ;}
    break;

  case 835:

/* Line 1455 of yacc.c  */
#line 4770 "parser.y"
    {
        //cb_error (_("ALTER statement is obsolete and unsupported"));
        { BEGIN_STATEMENT ("ALTER", 0); }
  ;}
    break;

  case 838:

/* Line 1455 of yacc.c  */
#line 4780 "parser.y"
    {
    cb_emit_alter((yyvsp[(2) - (5)]), (yyvsp[(5) - (5)]));
;}
    break;

  case 841:

/* Line 1455 of yacc.c  */
#line 4793 "parser.y"
    { 
                                  BEGIN_STATEMENT ("CALL", TERM_CALL); 
                                ;}
    break;

  case 842:

/* Line 1455 of yacc.c  */
#line 4803 "parser.y"
    {
        cb_emit_call ((yyvsp[(4) - (9)]), cb_merge_call_conv((yyvsp[(3) - (9)]), (yyvsp[(5) - (9)])), current_statement->call_using_save, 
                      current_statement->call_returning_save, (yyvsp[(7) - (9)]), (yyvsp[(8) - (9)]));
  ;}
    break;

  case 843:

/* Line 1455 of yacc.c  */
#line 4810 "parser.y"
    { BEGIN_STATEMENT ("CHAIN", TERM_CHAIN); ;}
    break;

  case 844:

/* Line 1455 of yacc.c  */
#line 4818 "parser.y"
    {
        
        cb_emit_chain ((yyvsp[(4) - (9)]), cb_merge_call_conv((yyvsp[(3) - (9)]), (yyvsp[(5) - (9)])), (yyvsp[(6) - (9)]), current_program->cb_return_code, (yyvsp[(7) - (9)]), (yyvsp[(8) - (9)]));
        cb_emit_stop_run (current_program->cb_return_code);
  ;}
    break;

  case 847:

/* Line 1455 of yacc.c  */
#line 4829 "parser.y"
    { current_statement->call_using_save = (yyvsp[(1) - (1)]); ;}
    break;

  case 848:

/* Line 1455 of yacc.c  */
#line 4830 "parser.y"
    { current_statement->call_returning_save = (yyvsp[(1) - (1)]) ;}
    break;

  case 849:

/* Line 1455 of yacc.c  */
#line 4836 "parser.y"
    {
    (yyval) = NULL; 
    if (cb_flag_defaultcall) {
        (yyval) = cb_int(cb_flag_defaultcall);
        }
    ;}
    break;

  case 851:

/* Line 1455 of yacc.c  */
#line 4847 "parser.y"
    { 
    (yyval) = NULL; 
  ;}
    break;

  case 852:

/* Line 1455 of yacc.c  */
#line 4851 "parser.y"
    {
      (yyval) = cb_build_pair ((yyvsp[(2) - (4)]), (yyvsp[(3) - (4)]));
  ;}
    break;

  case 853:

/* Line 1455 of yacc.c  */
#line 4855 "parser.y"
    {
      (yyval) = cb_build_pair (cb_int(CB_CALL_STATIC), (yyvsp[(2) - (3)]));
  ;}
    break;

  case 854:

/* Line 1455 of yacc.c  */
#line 4859 "parser.y"
    {
      (yyval) = cb_build_pair (cb_int(CB_CALL_DYNAMIC), cb_int(CB_CALL_STDCALL));
  ;}
    break;

  case 855:

/* Line 1455 of yacc.c  */
#line 4865 "parser.y"
    { (yyval) = cb_int(CB_CALL_CDECL); ;}
    break;

  case 856:

/* Line 1455 of yacc.c  */
#line 4866 "parser.y"
    { (yyval) = cb_int(CB_CALL_CDECL); ;}
    break;

  case 857:

/* Line 1455 of yacc.c  */
#line 4867 "parser.y"
    { (yyval) = cb_int(CB_CALL_STDCALL); ;}
    break;

  case 858:

/* Line 1455 of yacc.c  */
#line 4872 "parser.y"
    { (yyval) = cb_int(CB_CALL_DYNAMIC); ;}
    break;

  case 859:

/* Line 1455 of yacc.c  */
#line 4873 "parser.y"
    { (yyval) = cb_int(CB_CALL_STATIC); ;}
    break;

  case 860:

/* Line 1455 of yacc.c  */
#line 4877 "parser.y"
    { (yyval) = NULL; ;}
    break;

  case 861:

/* Line 1455 of yacc.c  */
#line 4878 "parser.y"
    { (yyval) = (yyvsp[(1) - (1)]) ; ;}
    break;

  case 862:

/* Line 1455 of yacc.c  */
#line 4883 "parser.y"
    {
    call_mode = CB_CALL_BY_DEFAULT;
    if (cb_flag_value_size_auto)
      size_mode = CB_SIZE_AUTO;
    else
      size_mode = CB_SIZE_4;
  ;}
    break;

  case 863:

/* Line 1455 of yacc.c  */
#line 4890 "parser.y"
    { (yyval) = (yyvsp[(3) - (3)]); ;}
    break;

  case 864:

/* Line 1455 of yacc.c  */
#line 4895 "parser.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 865:

/* Line 1455 of yacc.c  */
#line 4897 "parser.y"
    { (yyval) = cb_list_append ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])); ;}
    break;

  case 866:

/* Line 1455 of yacc.c  */
#line 4902 "parser.y"
    {
    if (CB_CALL_BY(call_mode) != CB_CALL_BY_REFERENCE && CB_CALL_BY(call_mode) != CB_CALL_BY_DEFAULT) {
                cb_error (_("OMITTED only allowed with BY REFERENCE"));
        }
    (yyval) = cb_build_pair (cb_int (call_mode), cb_null);
  ;}
    break;

  case 867:

/* Line 1455 of yacc.c  */
#line 4909 "parser.y"
    {
    (yyval) = cb_build_pair (cb_int (call_mode), (yyvsp[(3) - (5)]) );
    CB_SIZES ((yyval)) = size_mode;
  ;}
    break;

  case 869:

/* Line 1455 of yacc.c  */
#line 4918 "parser.y"
    {
    call_mode = CB_CALL_BY_REFERENCE;
  ;}
    break;

  case 870:

/* Line 1455 of yacc.c  */
#line 4922 "parser.y"
    {
    if (current_program->flag_chained) {
                cb_error (_("BY CONTENT not allowed in CHAINED program"));
        } else {
        call_mode = CB_CALL_BY_CONTENT;
    }
  ;}
    break;

  case 871:

/* Line 1455 of yacc.c  */
#line 4930 "parser.y"
    {
    if (current_program->flag_chained) {
                cb_error (_("BY DESCRIPTOR not allowed in CHAINED program"));
        } else {
        call_mode = CB_CALL_BY_DESCRIPTOR;
    }
  ;}
    break;

  case 872:

/* Line 1455 of yacc.c  */
#line 4938 "parser.y"
    {
    if (current_program->flag_chained) {
                cb_error (_("BY VALUE not allowed in CHAINED program"));
        } else {
        call_mode = CB_CALL_BY_VALUE;
    }
  ;}
    break;

  case 873:

/* Line 1455 of yacc.c  */
#line 4955 "parser.y"
    { (yyval) = cb_build_pair(cb_int0 , (yyvsp[(2) - (2)])); ;}
    break;

  case 874:

/* Line 1455 of yacc.c  */
#line 4956 "parser.y"
    { (yyval) = cb_build_pair(cb_int(CB_CALL_RETURN_INTO), (yyvsp[(3) - (3)])); ;}
    break;

  case 875:

/* Line 1455 of yacc.c  */
#line 4957 "parser.y"
    { (yyval) = cb_build_pair(cb_int(CB_CALL_RETURN_ADDROF), (yyvsp[(4) - (4)])); ;}
    break;

  case 878:

/* Line 1455 of yacc.c  */
#line 4967 "parser.y"
    {
    (yyval) = NULL;
  ;}
    break;

  case 879:

/* Line 1455 of yacc.c  */
#line 4971 "parser.y"
    {
    check_unreached = 0;
  ;}
    break;

  case 880:

/* Line 1455 of yacc.c  */
#line 4975 "parser.y"
    {
    (yyval) = (yyvsp[(3) - (3)]);
  ;}
    break;

  case 881:

/* Line 1455 of yacc.c  */
#line 4982 "parser.y"
    {
    (yyval) = NULL;
  ;}
    break;

  case 882:

/* Line 1455 of yacc.c  */
#line 4986 "parser.y"
    {
    check_unreached = 0;
  ;}
    break;

  case 883:

/* Line 1455 of yacc.c  */
#line 4990 "parser.y"
    {
    (yyval) = (yyvsp[(3) - (3)]);
  ;}
    break;

  case 884:

/* Line 1455 of yacc.c  */
#line 4996 "parser.y"
    { terminator_warning (TERM_CALL); ;}
    break;

  case 885:

/* Line 1455 of yacc.c  */
#line 4997 "parser.y"
    { terminator_clear (TERM_CALL); ;}
    break;

  case 886:

/* Line 1455 of yacc.c  */
#line 5001 "parser.y"
    { terminator_warning (TERM_CHAIN); ;}
    break;

  case 887:

/* Line 1455 of yacc.c  */
#line 5002 "parser.y"
    { terminator_clear (TERM_CHAIN); ;}
    break;

  case 888:

/* Line 1455 of yacc.c  */
#line 5011 "parser.y"
    { BEGIN_STATEMENT ("CANCEL", 0); ;}
    break;

  case 891:

/* Line 1455 of yacc.c  */
#line 5017 "parser.y"
    {
        cb_emit_cancel ((yyvsp[(2) - (2)]));
  ;}
    break;

  case 892:

/* Line 1455 of yacc.c  */
#line 5028 "parser.y"
    {                BEGIN_STATEMENT ("CHECKPOINT", 0); ;}
    break;

  case 894:

/* Line 1455 of yacc.c  */
#line 5034 "parser.y"
    { 
             if (!cb_flag_context_reload_enable) {
                 cb_error (_("CHECKPOINT is only valid with -fcheckpoint compilation flag"));
             }
             if ((yyvsp[(1) - (1)]))
                 cb_emit_set_context_filename ((yyvsp[(1) - (1)]));
      ;}
    break;

  case 895:

/* Line 1455 of yacc.c  */
#line 5042 "parser.y"
    {
            cb_emit_perform (cb_build_perform_once(NULL), 
                             cb_build_pair (cb_save_context_handler_ref, cb_save_context_handler_ref));
            cb_emit_move (cb_zero, cb_list_init(current_program->cb_return_code));
      ;}
    break;

  case 896:

/* Line 1455 of yacc.c  */
#line 5048 "parser.y"
    {
            if ((yyvsp[(5) - (5)])) {
                cb_emit_move (current_program->cb_context_code, cb_list_init((yyvsp[(5) - (5)])));
            }
      ;}
    break;

  case 898:

/* Line 1455 of yacc.c  */
#line 5057 "parser.y"
    {
      cb_emit_init_context_save (cb_zero);
;}
    break;

  case 899:

/* Line 1455 of yacc.c  */
#line 5061 "parser.y"
    {
      cb_emit_init_context_save (cb_one);
      cb_emit_move ((yyvsp[(2) - (2)]), cb_list_init(current_program->cb_return_code));
;}
    break;

  case 900:

/* Line 1455 of yacc.c  */
#line 5068 "parser.y"
    { (yyval) = cb_zero; ;}
    break;

  case 901:

/* Line 1455 of yacc.c  */
#line 5069 "parser.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 902:

/* Line 1455 of yacc.c  */
#line 5070 "parser.y"
    { (yyval) = (yyvsp[(2) - (2)]); ;}
    break;

  case 903:

/* Line 1455 of yacc.c  */
#line 5071 "parser.y"
    { (yyval) = (yyvsp[(2) - (2)]); ;}
    break;

  case 904:

/* Line 1455 of yacc.c  */
#line 5075 "parser.y"
    { (yyval) = NULL; ;}
    break;

  case 905:

/* Line 1455 of yacc.c  */
#line 5076 "parser.y"
    { (yyval) = (yyvsp[(2) - (2)]); ;}
    break;

  case 906:

/* Line 1455 of yacc.c  */
#line 5084 "parser.y"
    { BEGIN_STATEMENT ("CLOSE", 0); ;}
    break;

  case 909:

/* Line 1455 of yacc.c  */
#line 5091 "parser.y"
    {
        BEGIN_IMPLICIT_STATEMENT ();
    if ((yyvsp[(2) - (3)]) != cb_error_node) {
        cb_emit_close ((yyvsp[(2) - (3)]), (yyvsp[(3) - (3)]));
  }
  ;}
    break;

  case 910:

/* Line 1455 of yacc.c  */
#line 5100 "parser.y"
    { (yyval) = cb_int (COB_CLOSE_NORMAL); ;}
    break;

  case 911:

/* Line 1455 of yacc.c  */
#line 5101 "parser.y"
    { (yyval) = cb_int (COB_CLOSE_UNIT); ;}
    break;

  case 912:

/* Line 1455 of yacc.c  */
#line 5102 "parser.y"
    { (yyval) = cb_int (COB_CLOSE_UNIT_REMOVAL); ;}
    break;

  case 913:

/* Line 1455 of yacc.c  */
#line 5103 "parser.y"
    { (yyval) = cb_int (COB_CLOSE_NO_REWIND); ;}
    break;

  case 914:

/* Line 1455 of yacc.c  */
#line 5104 "parser.y"
    { (yyval) = cb_int (COB_CLOSE_LOCK); ;}
    break;

  case 917:

/* Line 1455 of yacc.c  */
#line 5115 "parser.y"
    { BEGIN_STATEMENT ("COMPUTE", TERM_COMPUTE); rounded_used = 0; ;}
    break;

  case 919:

/* Line 1455 of yacc.c  */
#line 5122 "parser.y"
    {
   /*CIT*/
        if (current_statement->handler1 || current_statement->handler2) {
            cb_check_optimized_field((yyvsp[(1) - (4)]), 0);
        }
        else 
        {
           if (CB_EXCEPTION_ENABLE (COB_EC_SIZE_ZERO_DIVIDE)) {
               current_statement->handler_id = COB_EC_SIZE_ZERO_DIVIDE;
           } 
           if (CB_EXCEPTION_ENABLE (COB_EC_SIZE_OVERFLOW)) {
               if (current_statement->handler_id == 0)
                  current_statement->handler_id = COB_EC_SIZE_OVERFLOW;
               else 
                  current_statement->handler_id = COB_EC_SIZE;
           }
           cb_check_optimized_field((yyvsp[(1) - (4)]), 1);
        }        
        cb_emit_arithmetic ((yyvsp[(1) - (4)]), 0, (yyvsp[(3) - (4)]), !rounded_used);
  ;}
    break;

  case 920:

/* Line 1455 of yacc.c  */
#line 5145 "parser.y"
    { terminator_warning (TERM_COMPUTE); ;}
    break;

  case 921:

/* Line 1455 of yacc.c  */
#line 5146 "parser.y"
    { terminator_clear (TERM_COMPUTE); ;}
    break;

  case 924:

/* Line 1455 of yacc.c  */
#line 5157 "parser.y"
    {
        BEGIN_STATEMENT ("COMMIT", 0);
        cb_emit_commit ();
  ;}
    break;

  case 925:

/* Line 1455 of yacc.c  */
#line 5170 "parser.y"
    {
        BEGIN_STATEMENT ("CONTINUE", 0);
        cb_emit_continue ();
  ;}
    break;

  case 926:

/* Line 1455 of yacc.c  */
#line 5182 "parser.y"
    { BEGIN_STATEMENT ("DELETE", TERM_DELETE); ;}
    break;

  case 927:

/* Line 1455 of yacc.c  */
#line 5185 "parser.y"
    {
    if ((yyvsp[(3) - (6)]) != cb_error_node) {
        cb_emit_delete ((yyvsp[(3) - (6)]));
  }
  ;}
    break;

  case 928:

/* Line 1455 of yacc.c  */
#line 5190 "parser.y"
    { BEGIN_STATEMENT ("DELETE", TERM_DELETE); ;}
    break;

  case 929:

/* Line 1455 of yacc.c  */
#line 5193 "parser.y"
    {
    if ((yyvsp[(3) - (5)]) != cb_error_node) {
        cb_emit_delete_file ((yyvsp[(4) - (5)]));
  }
  ;}
    break;

  case 930:

/* Line 1455 of yacc.c  */
#line 5201 "parser.y"
    { terminator_warning (TERM_DELETE); ;}
    break;

  case 931:

/* Line 1455 of yacc.c  */
#line 5202 "parser.y"
    { terminator_clear (TERM_DELETE); ;}
    break;

  case 932:

/* Line 1455 of yacc.c  */
#line 5212 "parser.y"
    {
        BEGIN_STATEMENT ("DISPLAY", TERM_DISPLAY);
        dispattrs = 0;
        dispprompt = 0;
        display_fgc = NULL;
        display_bgc = NULL;
        display_line = NULL;
        display_column = NULL;
        scroll = NULL;
        display_advance = cb_int1;
        display_x_list = NULL;
        display_upon = cb_int(COB_DEVICE_SYSOUT);
        acc_exception_field = NULL;
  ;}
    break;

  case 934:

/* Line 1455 of yacc.c  */
#line 5232 "parser.y"
    {
    cb_emit_env_name ((yyvsp[(1) - (3)]));
  ;}
    break;

  case 935:

/* Line 1455 of yacc.c  */
#line 5236 "parser.y"
    {
    cb_emit_env_value ((yyvsp[(1) - (3)]));
  ;}
    break;

  case 936:

/* Line 1455 of yacc.c  */
#line 5240 "parser.y"
    {
    cb_emit_arg_number ((yyvsp[(1) - (3)]));
  ;}
    break;

  case 937:

/* Line 1455 of yacc.c  */
#line 5244 "parser.y"
    {
    cb_emit_command_line ((yyvsp[(1) - (3)]));
  ;}
    break;

  case 938:

/* Line 1455 of yacc.c  */
#line 5248 "parser.y"
    {
    cb_emit_display (display_x_list, display_upon, display_advance, display_line, display_column, display_fgc, display_bgc, scroll, dispattrs);
  ;}
    break;

  case 941:

/* Line 1455 of yacc.c  */
#line 5273 "parser.y"
    { display_x_list = cb_list_add ( display_x_list, (yyvsp[(1) - (1)])); ;}
    break;

  case 943:

/* Line 1455 of yacc.c  */
#line 5275 "parser.y"
    { display_upon = cb_build_display_upon ((yyvsp[(2) - (2)])); ;}
    break;

  case 944:

/* Line 1455 of yacc.c  */
#line 5276 "parser.y"
    { display_upon = cb_build_display_upon_direct ((yyvsp[(2) - (2)])); ;}
    break;

  case 945:

/* Line 1455 of yacc.c  */
#line 5277 "parser.y"
    { display_upon = cb_int(COB_DEVICE_PRINTER); ;}
    break;

  case 946:

/* Line 1455 of yacc.c  */
#line 5278 "parser.y"
    { display_upon = cb_flag_console_equal_sysfile ? cb_int(COB_DEVICE_SYSOUT) : cb_int(COB_DEVICE_CONSOLE); ;}
    break;

  case 947:

/* Line 1455 of yacc.c  */
#line 5279 "parser.y"
    { display_advance = cb_int0; ;}
    break;

  case 948:

/* Line 1455 of yacc.c  */
#line 5280 "parser.y"
    { cb_warning (_("CONVERT ignored in DISPLAY/ACCEPT"));;}
    break;

  case 949:

/* Line 1455 of yacc.c  */
#line 5281 "parser.y"
    { dispattrs |= COB_SCREEN_BELL; ;}
    break;

  case 950:

/* Line 1455 of yacc.c  */
#line 5282 "parser.y"
    { dispattrs |= COB_SCREEN_NO_BELL; ;}
    break;

  case 951:

/* Line 1455 of yacc.c  */
#line 5283 "parser.y"
    { dispattrs |= COB_SCREEN_BLINK; ;}
    break;

  case 952:

/* Line 1455 of yacc.c  */
#line 5284 "parser.y"
    { dispattrs |= COB_SCREEN_ERASE_EOL; ;}
    break;

  case 953:

/* Line 1455 of yacc.c  */
#line 5285 "parser.y"
    { dispattrs |= COB_SCREEN_ERASE_EOS; ;}
    break;

  case 954:

/* Line 1455 of yacc.c  */
#line 5286 "parser.y"
    { dispattrs |= COB_SCREEN_HIGHLIGHT; ;}
    break;

  case 955:

/* Line 1455 of yacc.c  */
#line 5287 "parser.y"
    { dispattrs |= COB_SCREEN_LOWLIGHT; ;}
    break;

  case 956:

/* Line 1455 of yacc.c  */
#line 5288 "parser.y"
    { dispattrs |= COB_SCREEN_REVERSE; ;}
    break;

  case 957:

/* Line 1455 of yacc.c  */
#line 5289 "parser.y"
    { dispattrs |= COB_SCREEN_UNDERLINE; ;}
    break;

  case 958:

/* Line 1455 of yacc.c  */
#line 5290 "parser.y"
    { dispattrs |= COB_SCREEN_OVERLINE; ;}
    break;

  case 959:

/* Line 1455 of yacc.c  */
#line 5292 "parser.y"
    {
        display_fgc = (yyvsp[(4) - (4)]);
  ;}
    break;

  case 960:

/* Line 1455 of yacc.c  */
#line 5296 "parser.y"
    {
        display_bgc = (yyvsp[(4) - (4)]);
  ;}
    break;

  case 961:

/* Line 1455 of yacc.c  */
#line 5300 "parser.y"
    {
    scroll = (yyvsp[(3) - (4)]);
  ;}
    break;

  case 962:

/* Line 1455 of yacc.c  */
#line 5304 "parser.y"
    {
    dispattrs |= COB_SCREEN_SCROLL_DOWN;
    scroll = (yyvsp[(3) - (4)]);
  ;}
    break;

  case 963:

/* Line 1455 of yacc.c  */
#line 5308 "parser.y"
    { dispattrs |= COB_SCREEN_BLANK_LINE; ;}
    break;

  case 964:

/* Line 1455 of yacc.c  */
#line 5309 "parser.y"
    { dispattrs |= COB_SCREEN_BLANK_SCREEN; ;}
    break;

  case 965:

/* Line 1455 of yacc.c  */
#line 5313 "parser.y"
    { terminator_warning (TERM_DISPLAY); ;}
    break;

  case 966:

/* Line 1455 of yacc.c  */
#line 5314 "parser.y"
    { terminator_clear (TERM_DISPLAY); ;}
    break;

  case 967:

/* Line 1455 of yacc.c  */
#line 5323 "parser.y"
    { BEGIN_STATEMENT ("DIVIDE", TERM_DIVIDE); 
                                  rounded_used = 0;   
                                ;}
    break;

  case 969:

/* Line 1455 of yacc.c  */
#line 5332 "parser.y"
    {
   /*CIT*/
        void *a1 = (yyvsp[(1) - (4)]);
        if (CB_EXCEPTION_ENABLE (COB_EC_SIZE_ZERO_DIVIDE)) {
            current_statement->handler_id = COB_EC_SIZE_ZERO_DIVIDE;
        }

        if (current_statement->handler1 || current_statement->handler2) {
            a1 = cb_check_optimized_field_1((yyvsp[(1) - (4)]));
            cb_check_optimized_field((yyvsp[(3) - (4)]), 0);
        } else {
            cb_check_optimized_field((yyvsp[(3) - (4)]), 1);
        }
        cb_emit_arithmetic ((yyvsp[(3) - (4)]), '/', a1, !rounded_used);
  ;}
    break;

  case 970:

/* Line 1455 of yacc.c  */
#line 5348 "parser.y"
    {
   /*CIT*/
        void * a1 = (yyvsp[(1) - (6)]);
        void * a3 = (yyvsp[(3) - (6)]);
        if (CB_EXCEPTION_ENABLE (COB_EC_SIZE_ZERO_DIVIDE)) {
            current_statement->handler_id = COB_EC_SIZE_ZERO_DIVIDE;
        }
        if (current_statement->handler1 || current_statement->handler2) {
            a1 = cb_check_optimized_field_1((yyvsp[(1) - (6)]));
            a3 = cb_check_optimized_field_1((yyvsp[(3) - (6)]));
            cb_check_optimized_field((yyvsp[(5) - (6)]), 0);
        } else {
            cb_check_optimized_field((yyvsp[(5) - (6)]), 1);
        }
        cb_emit_arithmetic ((yyvsp[(5) - (6)]), 0, cb_build_binary_op (a3, '/', a1), !rounded_used);
  ;}
    break;

  case 971:

/* Line 1455 of yacc.c  */
#line 5365 "parser.y"
    {
        void * a1 = (yyvsp[(1) - (6)]);
        void * a3 = (yyvsp[(3) - (6)]);
   /*CIT*/
        if (CB_EXCEPTION_ENABLE (COB_EC_SIZE_ZERO_DIVIDE)) {
            current_statement->handler_id = COB_EC_SIZE_ZERO_DIVIDE;
        }
        if (current_statement->handler1 || current_statement->handler2) {
            a1 = cb_check_optimized_field_1((yyvsp[(1) - (6)]));
            a3 = cb_check_optimized_field_1((yyvsp[(3) - (6)]));
            cb_check_optimized_field((yyvsp[(5) - (6)]), 0);
        } else {
            cb_check_optimized_field((yyvsp[(5) - (6)]), 1);
        }
        cb_emit_arithmetic ((yyvsp[(5) - (6)]), 0, cb_build_binary_op (a1, '/', a3), !rounded_used);
  ;}
    break;

  case 972:

/* Line 1455 of yacc.c  */
#line 5382 "parser.y"
    {
        void * a1;
        void * a3;
        if (CB_EXCEPTION_ENABLE (COB_EC_SIZE_ZERO_DIVIDE)) {
            current_statement->handler_id = COB_EC_SIZE_ZERO_DIVIDE;
        }
        a1 = cb_check_optimized_field_1((yyvsp[(1) - (8)]));
        a3 = cb_check_optimized_field_1((yyvsp[(3) - (8)]));
        cb_check_optimized_field((yyvsp[(5) - (8)]), 0);
        cb_check_optimized_field((yyvsp[(7) - (8)]), 0);
        cb_emit_divide (a3, a1, (yyvsp[(5) - (8)]), (yyvsp[(7) - (8)]));
  ;}
    break;

  case 973:

/* Line 1455 of yacc.c  */
#line 5395 "parser.y"
    {
        void * a1;
        void * a3;
        if (CB_EXCEPTION_ENABLE (COB_EC_SIZE_ZERO_DIVIDE)) {
            current_statement->handler_id = COB_EC_SIZE_ZERO_DIVIDE;
        }
        a1 = cb_check_optimized_field_1((yyvsp[(1) - (8)]));
        a3 = cb_check_optimized_field_1((yyvsp[(3) - (8)]));
        cb_check_optimized_field((yyvsp[(5) - (8)]), 0);
        cb_check_optimized_field((yyvsp[(7) - (8)]), 0);
        cb_emit_divide (a1, a3, (yyvsp[(5) - (8)]), (yyvsp[(7) - (8)]));
  ;}
    break;

  case 974:

/* Line 1455 of yacc.c  */
#line 5410 "parser.y"
    { terminator_warning (TERM_DIVIDE); ;}
    break;

  case 975:

/* Line 1455 of yacc.c  */
#line 5411 "parser.y"
    { terminator_clear (TERM_DIVIDE); ;}
    break;

  case 976:

/* Line 1455 of yacc.c  */
#line 5420 "parser.y"
    { BEGIN_STATEMENT ("ENTRY", 0); ;}
    break;

  case 977:

/* Line 1455 of yacc.c  */
#line 5422 "parser.y"
    {
    if (current_program->nested_level) {
        cb_error (_("ENTRY is invalid in nested program"));
    } else if (cb_verify (cb_entry_statement, "ENTRY")) {
        if (cobc_check_valid_name ((char *)(CB_LITERAL ((yyvsp[(3) - (6)]))->data))) {
                        cb_error (_("ENTRY '%s' invalid, may be a reserved C language word or function name."), (char *)(CB_LITERAL ((yyvsp[(3) - (6)]))->data));
                }
                emit_entry ((char *)(CB_LITERAL ((yyvsp[(3) - (6)]))->data), 1, (yyvsp[(5) - (6)]), cb_merge_call_conv((yyvsp[(4) - (6)]), NULL), 0);
        }
    check_unreached = 0;
  ;}
    break;

  case 978:

/* Line 1455 of yacc.c  */
#line 5436 "parser.y"
    { (yyval) = NULL; ;}
    break;

  case 979:

/* Line 1455 of yacc.c  */
#line 5437 "parser.y"
    { (yyval) = (yyvsp[(1) - (1)]) ; ;}
    break;

  case 980:

/* Line 1455 of yacc.c  */
#line 5442 "parser.y"
    {
    call_mode = CB_CALL_BY_REFERENCE;
    size_mode = CB_SIZE_4;
  ;}
    break;

  case 981:

/* Line 1455 of yacc.c  */
#line 5446 "parser.y"
    { (yyval) = (yyvsp[(3) - (3)]); ;}
    break;

  case 982:

/* Line 1455 of yacc.c  */
#line 5451 "parser.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 983:

/* Line 1455 of yacc.c  */
#line 5453 "parser.y"
    { (yyval) = cb_list_append ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])); ;}
    break;

  case 984:

/* Line 1455 of yacc.c  */
#line 5458 "parser.y"
    {
    if (CB_CALL_BY(call_mode) != CB_CALL_BY_REFERENCE) {
                cb_error (_("OMITTED only allowed with BY REFERENCE"));
        }
    (yyval) = cb_build_pair (cb_int (call_mode), cb_null);
  ;}
    break;

  case 985:

/* Line 1455 of yacc.c  */
#line 5465 "parser.y"
    {
    cb_tree x = NULL;
    if (current_program->flag_is_external) {
       struct cb_field *f = cb_type((yyvsp[(3) - (5)]));
       if (f && f->flag_is_typedef) {
            x = CB_TREE(f);
       } 
    } else {
        x = cb_build_identifier((yyvsp[(3) - (5)]));
    }
    if (!x)
        cb_error (_("Invalid entry parameter "));
    (yyval) = cb_build_pair (cb_int (call_mode), x );
    CB_SIZES ((yyval)) = size_mode;
  ;}
    break;

  case 986:

/* Line 1455 of yacc.c  */
#line 5481 "parser.y"
    {
    if (!current_program->flag_is_external) {
                cb_error (_("ANY only allowed with program-id is external declaration "));
    }
    (yyval) = cb_build_pair (cb_int (call_mode), cb_null );
    CB_SIZES ((yyval)) = size_mode;
  ;}
    break;

  case 987:

/* Line 1455 of yacc.c  */
#line 5491 "parser.y"
    { (yyval) = NULL;;}
    break;

  case 988:

/* Line 1455 of yacc.c  */
#line 5493 "parser.y"
    {
    cb_tree x = NULL;
    struct cb_field *f = cb_type((yyvsp[(2) - (2)]));
    if (f && f->flag_is_typedef) {
        if (current_program->flag_is_external) {
            if (f->size > sizeof(void*)) {
                cb_error (_("RETURNING size not supported"));
            } else {
                x = CB_TREE(f);
            }
        }
    } 
    if (!x)
        cb_error (_("Invalid RETURNING : ENTRY only support TYPE returning "));
    (yyval) = x;
  ;}
    break;

  case 989:

/* Line 1455 of yacc.c  */
#line 5517 "parser.y"
    {
        BEGIN_STATEMENT ("EVALUATE", TERM_EVALUATE);
        eval_level++;
        for (eval_inc = 0; eval_inc < 64; eval_inc++) {
                eval_check[eval_level][eval_inc] = 0;
        }
        eval_inc = 0;
        eval_inc2 = 0;
  ;}
    break;

  case 990:

/* Line 1455 of yacc.c  */
#line 5528 "parser.y"
    {
        cb_emit_evaluate ((yyvsp[(3) - (5)]), (yyvsp[(4) - (5)]));
        eval_level--;
  ;}
    break;

  case 991:

/* Line 1455 of yacc.c  */
#line 5535 "parser.y"
    { (yyval) = cb_list_init ((yyvsp[(1) - (1)])); ;}
    break;

  case 992:

/* Line 1455 of yacc.c  */
#line 5537 "parser.y"
    { (yyval) = cb_list_add ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])); ;}
    break;

  case 993:

/* Line 1455 of yacc.c  */
#line 5542 "parser.y"
    {
        (yyval) = (yyvsp[(1) - (1)]);
        /*eval_subject[eval_level][eval_inc] = $1;*/
        if (CB_REFERENCE_P((yyvsp[(1) - (1)]))) {
                eval_check[eval_level][eval_inc++] = 0;
        } else {
                eval_check[eval_level][eval_inc++] = 1;
        }
  ;}
    break;

  case 994:

/* Line 1455 of yacc.c  */
#line 5552 "parser.y"
    {
        (yyval) = cb_true;
        /*eval_subject[eval_level][eval_inc] = cb_true;*/
        eval_check[eval_level][eval_inc++] = 2;
  ;}
    break;

  case 995:

/* Line 1455 of yacc.c  */
#line 5558 "parser.y"
    {
        (yyval) = cb_false;
        /*eval_subject[eval_level][eval_inc] = cb_false;*/
        eval_check[eval_level][eval_inc++] = 3;
  ;}
    break;

  case 996:

/* Line 1455 of yacc.c  */
#line 5576 "parser.y"
    {
    (yyval) = (yyvsp[(1) - (1)]);
  ;}
    break;

  case 997:

/* Line 1455 of yacc.c  */
#line 5582 "parser.y"
    { (yyval) = cb_list_init ((yyvsp[(1) - (1)])); ;}
    break;

  case 998:

/* Line 1455 of yacc.c  */
#line 5584 "parser.y"
    { (yyval) = cb_list_add ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])); ;}
    break;

  case 999:

/* Line 1455 of yacc.c  */
#line 5589 "parser.y"
    {
    check_unreached = 0;
  ;}
    break;

  case 1000:

/* Line 1455 of yacc.c  */
#line 5593 "parser.y"
    {
    if (cb_list_length((yyvsp[(3) - (3)])) == 0 || 
        (cb_list_length((yyvsp[(3) - (3)])) == 1 && CB_TREE((yyvsp[(3) - (3)]))->generated)) {
        cb_check_feature_x ((yyvsp[(1) - (3)]), cb_syntax_ibm5_2, "Empty WHEN/OTHER clause");
    }

    if (cb_is_other_caselist((yyvsp[(1) - (3)]))) {
        (yyval) = cb_cons ((yyvsp[(3) - (3)]), NULL);
    } else {
        (yyval) = cb_cons ((yyvsp[(3) - (3)]), (yyvsp[(1) - (3)]));
    }
    eval_inc2 = 0;
  ;}
    break;

  case 1001:

/* Line 1455 of yacc.c  */
#line 5609 "parser.y"
    { (yyval) = cb_list_init(cb_build_continue ());
      CB_TREE((yyval))->generated=1;
    ;}
    break;

  case 1002:

/* Line 1455 of yacc.c  */
#line 5613 "parser.y"
    { (yyval) = (yyvsp[(1) - (1)]);  ;}
    break;

  case 1003:

/* Line 1455 of yacc.c  */
#line 5617 "parser.y"
    { (yyval) = cb_list_init ((yyvsp[(2) - (2)])); eval_inc2 = 0;;}
    break;

  case 1004:

/* Line 1455 of yacc.c  */
#line 5619 "parser.y"
    { (yyval) = cb_list_add ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])); eval_inc2 = 0;;}
    break;

  case 1005:

/* Line 1455 of yacc.c  */
#line 5624 "parser.y"
    { (yyval) = cb_list_init (NULL);;}
    break;

  case 1006:

/* Line 1455 of yacc.c  */
#line 5625 "parser.y"
    { (yyval) = (yyvsp[(1) - (1)]);;}
    break;

  case 1007:

/* Line 1455 of yacc.c  */
#line 5629 "parser.y"
    { (yyval) = cb_list_init ((yyvsp[(1) - (1)]));  ;}
    break;

  case 1008:

/* Line 1455 of yacc.c  */
#line 5631 "parser.y"
    { (yyval) = cb_list_add ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])); ;}
    break;

  case 1009:

/* Line 1455 of yacc.c  */
#line 5636 "parser.y"
    {
        cb_tree oper;
        cb_tree e1;
        cb_tree e2;
        int subject_condition = 0;

        oper = cb_int0;
        e1 = (yyvsp[(1) - (2)]);
        e2 = (yyvsp[(2) - (2)]);

        /* in case the first token is NOT */
        if (CB_PURPOSE_INT (e1) == '!') {
                if (eval_check[eval_level][eval_inc2] < 2) {
                        oper = cb_int1;
                        e1 = CB_CHAIN (e1);
                }
        }
    /*CIT*/
        /* class or sign condition on subject */
        if ((eval_check[eval_level][eval_inc2] < 2) && !CB_CHAIN(e1))
            switch (CB_PURPOSE_INT (e1)) {
                case 'P': 
                    e1 = (oper == cb_int0 ? cb_positive : cb_not_positive); 
                    subject_condition = 1;
                    break;
                case 'N': 
                    e1 = (oper == cb_int0 ? cb_negative : cb_not_negative); 
                    subject_condition = 1;
                    break;
                case '9': 
                    e1 = (oper == cb_int0 ? cb_numeric : cb_not_numeric); 
                    subject_condition = 1;
                    break;
                case 'A': 
                    e1 = (oper == cb_int0 ? cb_alphabetic : cb_not_alphabetic); 
                    subject_condition = 1;
                    break;
                case 'L': 
                    e1 = (oper == cb_int0 ? cb_alphabetic_lower : cb_not_alphabetic_lower); 
                    subject_condition = 1;
                    break;
                case 'U': 
                    e1 = (oper == cb_int0 ? cb_alphabetic_upper : cb_not_alphabetic_upper); 
                    subject_condition = 1;
            }
    

        if (subject_condition) {
            (yyval) = e1;
            eval_inc2++;
        }
        else {
            /* in case the first token is Condition */
            switch (CB_PURPOSE_INT (e1)) {
            case '<':
            case '>':
            case '[':
            case ']':
            case '=':
            case '~':
                    if (eval_check[eval_level][eval_inc2] < 2) {
                            if (oper == cb_int1) {
                              switch (CB_PURPOSE_INT (e1)) {
                              case '<': oper = cb_int (']'); break;
                              case '>': oper = cb_int ('['); break;
                              case '[': oper = cb_int ('>'); break;
                              case ']': oper = cb_int ('<'); break;
                              case '=': oper = cb_int ('~'); break;
                              case '~': oper = cb_int ('='); break;
                              }
                            } else {
                               oper = CB_PURPOSE (e1) ;
                            }
                            e1 = CB_CHAIN (e1);
                    }
            }
        

            /* build expr now */
            e1 = cb_build_expr (e1);

            if (e2 == NULL) {
                    /* WHEN expr */
                    eval_inc2++;
                    (yyval) = cb_build_pair (oper, cb_build_pair (e1, NULL));
            } else {
                    /* WHEN expr THRU expr */
                    (yyval) = cb_build_pair (oper, cb_build_pair (e1, e2));
                    eval_inc2++;
            }            
        }
  ;}
    break;

  case 1010:

/* Line 1455 of yacc.c  */
#line 5728 "parser.y"
    { (yyval) = cb_any; eval_inc2++; ;}
    break;

  case 1011:

/* Line 1455 of yacc.c  */
#line 5729 "parser.y"
    { (yyval) = cb_true; eval_inc2++; ;}
    break;

  case 1012:

/* Line 1455 of yacc.c  */
#line 5730 "parser.y"
    { (yyval) = cb_false; eval_inc2++; ;}
    break;

  case 1013:

/* Line 1455 of yacc.c  */
#line 5734 "parser.y"
    { (yyval) = NULL; ;}
    break;

  case 1014:

/* Line 1455 of yacc.c  */
#line 5735 "parser.y"
    { (yyval) = (yyvsp[(2) - (2)]); ;}
    break;

  case 1015:

/* Line 1455 of yacc.c  */
#line 5739 "parser.y"
    { terminator_warning (TERM_EVALUATE); ;}
    break;

  case 1016:

/* Line 1455 of yacc.c  */
#line 5740 "parser.y"
    { terminator_clear (TERM_EVALUATE); ;}
    break;

  case 1017:

/* Line 1455 of yacc.c  */
#line 5749 "parser.y"
    { BEGIN_STATEMENT ("EXIT", 0); ;}
    break;

  case 1019:

/* Line 1455 of yacc.c  */
#line 5754 "parser.y"
    { /* nothing */ ;}
    break;

  case 1020:

/* Line 1455 of yacc.c  */
#line 5756 "parser.y"
    {
        if (in_declaratives && use_global_ind) {
            cb_error (_("EXIT PROGRAM is not allowed within a USE GLOBAL procedure"));
        }
        check_unreached = 1;
    ;}
    break;

  case 1022:

/* Line 1455 of yacc.c  */
#line 5764 "parser.y"
    {
        struct cb_perform       *p;
        char                    name[64];

        if (!perform_stack) {
                cb_error (_("EXIT PERFORM is only valid with inline PERFORM"));
        } else {
                p = CB_PERFORM (CB_VALUE (perform_stack));
                if (!p->exit_label) {
                        sprintf (name, "EXIT PERFORM %d", cb_id);
                        p->exit_label = cb_build_reference (name);
                        CB_LABEL (cb_build_label (p->exit_label, NULL))->need_begin = 1;
                }
        cb_emit_goto (cb_list_init (p->exit_label), NULL);
    }
  ;}
    break;

  case 1023:

/* Line 1455 of yacc.c  */
#line 5781 "parser.y"
    {
        struct cb_perform       *p;
        char                    name[64];

        if (!perform_stack) {
                cb_error (_("EXIT PERFORM is only valid with inline PERFORM"));
        } else {
                p = CB_PERFORM (CB_VALUE (perform_stack));
                if (!p->cycle_label) {
                        sprintf (name, "EXIT PERFORM CYCLE %d", cb_id);
                        p->cycle_label = cb_build_reference (name);
                        CB_LABEL (cb_build_label (p->cycle_label, NULL))->need_begin = 1;
                }
        cb_emit_goto (cb_list_init (p->cycle_label), NULL);
    }
  ;}
    break;

  case 1024:

/* Line 1455 of yacc.c  */
#line 5798 "parser.y"
    {
        cb_tree plabel;
        char    name[64];

        if (!current_section) {
            cb_error (_("EXIT SECTION is only valid with an active SECTION"));
        } else {
            if (!current_section->exit_label) {
                sprintf (name, "EXIT SECTION %d", cb_id);
                plabel = cb_build_reference(name);
                current_section->exit_label = cb_build_label (plabel, NULL);
                current_section->exit_label_ref = plabel;
                CB_LABEL (current_section->exit_label)->need_begin = 1;
                }
            cb_emit_goto (cb_list_init (current_section->exit_label_ref), NULL);
        }
  ;}
    break;

  case 1025:

/* Line 1455 of yacc.c  */
#line 5816 "parser.y"
    {
        cb_tree plabel;
        char    name[64];

        if (!current_paragraph) {
                cb_error (_("EXIT PARAGRAPH is only valid with an active PARAGRAPH"));
        } else {
                if (!current_paragraph->exit_label) {
                        sprintf (name, "EXIT PARAGRAPH %d", cb_id);
                        plabel = cb_build_reference(name);
                        current_paragraph->exit_label = cb_build_label (plabel, NULL);
                        current_paragraph->exit_label_ref = plabel;
                        CB_LABEL (current_paragraph->exit_label)->need_begin = 1;
                }
        cb_emit_goto (cb_list_init (current_paragraph->exit_label_ref), NULL);
    }
  ;}
    break;

  case 1026:

/* Line 1455 of yacc.c  */
#line 5836 "parser.y"
    {    cb_emit_exit (cb_flag_exit_program_forced, NULL); ;}
    break;

  case 1027:

/* Line 1455 of yacc.c  */
#line 5838 "parser.y"
    {          
        cb_emit_exit (cb_flag_exit_program_forced, (yyvsp[(2) - (2)]));
    ;}
    break;

  case 1028:

/* Line 1455 of yacc.c  */
#line 5849 "parser.y"
    {
        BEGIN_STATEMENT ("EXHIBIT", TERM_EXHIBIT);
        display_advance = cb_int1;
        display_upon = cb_int(COB_DEVICE_SYSOUT);
        exhibit_option = 0;
  ;}
    break;

  case 1029:

/* Line 1455 of yacc.c  */
#line 5856 "parser.y"
    {
    cb_emit_exhibit ((yyvsp[(4) - (5)]), display_upon, display_advance, exhibit_option);
  ;}
    break;

  case 1031:

/* Line 1455 of yacc.c  */
#line 5863 "parser.y"
    { (yyval) = cb_list_append (NULL, cb_build_exhibit_one((yyvsp[(1) - (1)]), exhibit_option)); ;}
    break;

  case 1032:

/* Line 1455 of yacc.c  */
#line 5864 "parser.y"
    { (yyval) = cb_list_append ((yyvsp[(1) - (2)]), cb_build_exhibit_one((yyvsp[(2) - (2)]), exhibit_option));   ;}
    break;

  case 1036:

/* Line 1455 of yacc.c  */
#line 5873 "parser.y"
    { exhibit_option |= 1; ;}
    break;

  case 1037:

/* Line 1455 of yacc.c  */
#line 5874 "parser.y"
    { exhibit_option |= 2; ;}
    break;

  case 1041:

/* Line 1455 of yacc.c  */
#line 5883 "parser.y"
    { display_upon = cb_build_display_upon ((yyvsp[(2) - (2)])); ;}
    break;

  case 1042:

/* Line 1455 of yacc.c  */
#line 5884 "parser.y"
    { display_upon = cb_build_display_upon_direct ((yyvsp[(2) - (2)])); ;}
    break;

  case 1043:

/* Line 1455 of yacc.c  */
#line 5885 "parser.y"
    { display_upon = cb_int(COB_DEVICE_SYSOUT); ;}
    break;

  case 1044:

/* Line 1455 of yacc.c  */
#line 5886 "parser.y"
    { display_upon = cb_flag_console_equal_sysfile ? cb_int(COB_DEVICE_SYSOUT) : cb_int(COB_DEVICE_CONSOLE); ;}
    break;

  case 1045:

/* Line 1455 of yacc.c  */
#line 5887 "parser.y"
    { display_advance = cb_int0; ;}
    break;

  case 1046:

/* Line 1455 of yacc.c  */
#line 5891 "parser.y"
    { terminator_warning (TERM_EXHIBIT); ;}
    break;

  case 1047:

/* Line 1455 of yacc.c  */
#line 5892 "parser.y"
    { terminator_clear (TERM_EXHIBIT); ;}
    break;

  case 1048:

/* Line 1455 of yacc.c  */
#line 5900 "parser.y"
    { BEGIN_STATEMENT ("FREE", 0); ;}
    break;

  case 1049:

/* Line 1455 of yacc.c  */
#line 5902 "parser.y"
    {
    cb_emit_free ((yyvsp[(3) - (3)]))
  ;}
    break;

  case 1050:

/* Line 1455 of yacc.c  */
#line 5913 "parser.y"
    { BEGIN_STATEMENT ("GENERATE", 0); ;}
    break;

  case 1051:

/* Line 1455 of yacc.c  */
#line 5915 "parser.y"
    {
    PENDING("GENERATE");
  ;}
    break;

  case 1052:

/* Line 1455 of yacc.c  */
#line 5926 "parser.y"
    { BEGIN_STATEMENT ("GO TO", 0); ;}
    break;

  case 1053:

/* Line 1455 of yacc.c  */
#line 5928 "parser.y"
    {
        cb_emit_goto ((yyvsp[(4) - (5)]), (yyvsp[(5) - (5)]));
  ;}
    break;

  case 1054:

/* Line 1455 of yacc.c  */
#line 5935 "parser.y"
    {
    check_unreached = 1;
    (yyval) = NULL;
  ;}
    break;

  case 1055:

/* Line 1455 of yacc.c  */
#line 5940 "parser.y"
    {
    check_unreached = 0;
    (yyval) = (yyvsp[(3) - (3)]);
  ;}
    break;

  case 1056:

/* Line 1455 of yacc.c  */
#line 5952 "parser.y"
    { BEGIN_STATEMENT ("GOBACK", 0); ;}
    break;

  case 1058:

/* Line 1455 of yacc.c  */
#line 5957 "parser.y"
    {
        check_unreached = 1;
        cb_emit_exit (1, NULL);
  ;}
    break;

  case 1059:

/* Line 1455 of yacc.c  */
#line 5962 "parser.y"
    {          
        cb_emit_exit (1, (yyvsp[(2) - (2)]));
    ;}
    break;

  case 1060:

/* Line 1455 of yacc.c  */
#line 5973 "parser.y"
    { BEGIN_STATEMENT ("IF", TERM_IF); ;}
    break;

  case 1062:

/* Line 1455 of yacc.c  */
#line 5979 "parser.y"
    {
    check_unreached = 0;
  ;}
    break;

  case 1063:

/* Line 1455 of yacc.c  */
#line 5983 "parser.y"
    {
    cb_emit_if ((yyvsp[(1) - (4)]), CB_PAIR_X((yyvsp[(4) - (4)])), CB_PAIR_Y((yyvsp[(4) - (4)])));
  ;}
    break;

  case 1065:

/* Line 1455 of yacc.c  */
#line 5991 "parser.y"
    {
    (yyval) = cb_build_pair (NULL, NULL);
    cb_check_feature_x (CB_TREE(current_statement), cb_syntax_ibm5_2, "Empty IF - END-IF");
  ;}
    break;

  case 1066:

/* Line 1455 of yacc.c  */
#line 5997 "parser.y"
    {
    (yyval) = cb_build_pair((yyvsp[(1) - (2)]),NULL);
  ;}
    break;

  case 1067:

/* Line 1455 of yacc.c  */
#line 6003 "parser.y"
    {
    (yyval) = cb_build_pair((yyvsp[(1) - (3)]), NULL);
    cb_check_feature_x (CB_TREE(current_statement), cb_syntax_ibm5_2, "Empty ELSE - END-IF");
  ;}
    break;

  case 1068:

/* Line 1455 of yacc.c  */
#line 6011 "parser.y"
    {
    (yyval) = cb_build_pair((yyvsp[(1) - (4)]), (yyvsp[(3) - (4)]));
  ;}
    break;

  case 1069:

/* Line 1455 of yacc.c  */
#line 6017 "parser.y"
    {
    (yyval) = cb_build_pair(NULL, (yyvsp[(2) - (3)]));
    cb_check_feature_x (CB_TREE(current_statement), cb_syntax_ibm5_2, "Empty IF - ELSE");
  ;}
    break;

  case 1070:

/* Line 1455 of yacc.c  */
#line 6023 "parser.y"
    {
    (yyval) = cb_build_pair (NULL, NULL);
    cb_check_feature_x (CB_TREE(current_statement), cb_syntax_ibm5_2, "Empty IF - ELSE - END-IF");
  ;}
    break;

  case 1071:

/* Line 1455 of yacc.c  */
#line 6030 "parser.y"
    { terminator_warning (TERM_IF); ;}
    break;

  case 1072:

/* Line 1455 of yacc.c  */
#line 6031 "parser.y"
    { terminator_clear (TERM_IF); ;}
    break;

  case 1073:

/* Line 1455 of yacc.c  */
#line 6039 "parser.y"
    { BEGIN_STATEMENT ("INITIALIZE", 0); ;}
    break;

  case 1074:

/* Line 1455 of yacc.c  */
#line 6041 "parser.y"
    {
        cb_emit_initialize_vars ((yyvsp[(3) - (7)]), (yyvsp[(4) - (7)]), (yyvsp[(5) - (7)]), (yyvsp[(6) - (7)]), (yyvsp[(7) - (7)]));
  ;}
    break;

  case 1075:

/* Line 1455 of yacc.c  */
#line 6047 "parser.y"
    { (yyval) = NULL; ;}
    break;

  case 1076:

/* Line 1455 of yacc.c  */
#line 6048 "parser.y"
    { (yyval) = cb_true; ;}
    break;

  case 1077:

/* Line 1455 of yacc.c  */
#line 6052 "parser.y"
    { if(cb_flag_initalize_to_value || cb_flag_initialize_to_value) (yyval) = cb_true; else (yyval) = NULL; ;}
    break;

  case 1078:

/* Line 1455 of yacc.c  */
#line 6053 "parser.y"
    { (yyval) = cb_true; ;}
    break;

  case 1079:

/* Line 1455 of yacc.c  */
#line 6054 "parser.y"
    { (yyval) = (yyvsp[(1) - (3)]); ;}
    break;

  case 1080:

/* Line 1455 of yacc.c  */
#line 6058 "parser.y"
    { (yyval) = NULL; ;}
    break;

  case 1081:

/* Line 1455 of yacc.c  */
#line 6060 "parser.y"
    { (yyval) = (yyvsp[(2) - (2)]); ;}
    break;

  case 1082:

/* Line 1455 of yacc.c  */
#line 6064 "parser.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 1083:

/* Line 1455 of yacc.c  */
#line 6066 "parser.y"
    { (yyval) = cb_list_append ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])); ;}
    break;

  case 1084:

/* Line 1455 of yacc.c  */
#line 6070 "parser.y"
    { (yyval) = cb_build_pair ((yyvsp[(1) - (4)]), (yyvsp[(4) - (4)])); ;}
    break;

  case 1085:

/* Line 1455 of yacc.c  */
#line 6074 "parser.y"
    { (yyval) = cb_int (CB_CATEGORY_ALPHABETIC); ;}
    break;

  case 1086:

/* Line 1455 of yacc.c  */
#line 6075 "parser.y"
    { (yyval) = cb_int (CB_CATEGORY_ALPHANUMERIC); ;}
    break;

  case 1087:

/* Line 1455 of yacc.c  */
#line 6076 "parser.y"
    { (yyval) = cb_int (CB_CATEGORY_NUMERIC); ;}
    break;

  case 1088:

/* Line 1455 of yacc.c  */
#line 6077 "parser.y"
    { (yyval) = cb_int (CB_CATEGORY_ALPHANUMERIC_EDITED); ;}
    break;

  case 1089:

/* Line 1455 of yacc.c  */
#line 6078 "parser.y"
    { (yyval) = cb_int (CB_CATEGORY_NUMERIC_EDITED); ;}
    break;

  case 1090:

/* Line 1455 of yacc.c  */
#line 6079 "parser.y"
    { (yyval) = cb_int (CB_CATEGORY_NATIONAL); ;}
    break;

  case 1091:

/* Line 1455 of yacc.c  */
#line 6080 "parser.y"
    { (yyval) = cb_int (CB_CATEGORY_NATIONAL_EDITED); ;}
    break;

  case 1092:

/* Line 1455 of yacc.c  */
#line 6084 "parser.y"
    { if(cb_flag_initalize_to_value || cb_flag_initialize_to_value) (yyval) = cb_true; else (yyval) = NULL; ;}
    break;

  case 1093:

/* Line 1455 of yacc.c  */
#line 6085 "parser.y"
    { (yyval) = cb_true; ;}
    break;

  case 1094:

/* Line 1455 of yacc.c  */
#line 6094 "parser.y"
    { BEGIN_STATEMENT ("INITIATE", 0); ;}
    break;

  case 1095:

/* Line 1455 of yacc.c  */
#line 6096 "parser.y"
    {
    PENDING("INITIATE");
  ;}
    break;

  case 1096:

/* Line 1455 of yacc.c  */
#line 6107 "parser.y"
    {
        BEGIN_STATEMENT ("INSPECT", 0);
        sending_id = 0;
        inspect_keyword = 0;
  ;}
    break;

  case 1098:

/* Line 1455 of yacc.c  */
#line 6116 "parser.y"
    { save_tree_1 = (yyvsp[(1) - (1)]); sending_id = 0; ;}
    break;

  case 1099:

/* Line 1455 of yacc.c  */
#line 6117 "parser.y"
    { save_tree_1 = (yyvsp[(1) - (1)]); sending_id = 1; ;}
    break;

  case 1100:

/* Line 1455 of yacc.c  */
#line 6118 "parser.y"
    { save_tree_1 = (yyvsp[(1) - (1)]); sending_id = 1; ;}
    break;

  case 1103:

/* Line 1455 of yacc.c  */
#line 6127 "parser.y"
    { cb_emit_inspect (save_tree_1, (yyvsp[(1) - (1)]), cb_int0, 0); ;}
    break;

  case 1104:

/* Line 1455 of yacc.c  */
#line 6128 "parser.y"
    { cb_emit_inspect (save_tree_1, (yyvsp[(1) - (1)]), cb_int1, 1); ;}
    break;

  case 1105:

/* Line 1455 of yacc.c  */
#line 6129 "parser.y"
    { cb_emit_inspect (save_tree_1, (yyvsp[(1) - (1)]), cb_int0, 2); ;}
    break;

  case 1106:

/* Line 1455 of yacc.c  */
#line 6135 "parser.y"
    { cb_init_tarrying (); ;}
    break;

  case 1107:

/* Line 1455 of yacc.c  */
#line 6136 "parser.y"
    { (yyval) = (yyvsp[(3) - (3)]); ;}
    break;

  case 1108:

/* Line 1455 of yacc.c  */
#line 6140 "parser.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 1109:

/* Line 1455 of yacc.c  */
#line 6141 "parser.y"
    { (yyval) = cb_list_append ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])); ;}
    break;

  case 1110:

/* Line 1455 of yacc.c  */
#line 6145 "parser.y"
    { (yyval) = cb_build_tarrying_data ((yyvsp[(1) - (2)])); ;}
    break;

  case 1111:

/* Line 1455 of yacc.c  */
#line 6146 "parser.y"
    { (yyval) = cb_build_tarrying_characters ((yyvsp[(2) - (2)])); ;}
    break;

  case 1112:

/* Line 1455 of yacc.c  */
#line 6147 "parser.y"
    { (yyval) = cb_build_tarrying_all (); ;}
    break;

  case 1113:

/* Line 1455 of yacc.c  */
#line 6148 "parser.y"
    { (yyval) = cb_build_tarrying_leading (); ;}
    break;

  case 1114:

/* Line 1455 of yacc.c  */
#line 6149 "parser.y"
    { (yyval) = cb_build_tarrying_trailing (); ;}
    break;

  case 1115:

/* Line 1455 of yacc.c  */
#line 6150 "parser.y"
    { (yyval) = cb_build_tarrying_value ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])); ;}
    break;

  case 1116:

/* Line 1455 of yacc.c  */
#line 6156 "parser.y"
    { (yyval) = (yyvsp[(2) - (2)]); inspect_keyword = 0; ;}
    break;

  case 1117:

/* Line 1455 of yacc.c  */
#line 6160 "parser.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 1118:

/* Line 1455 of yacc.c  */
#line 6161 "parser.y"
    { (yyval) = cb_list_append ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])); ;}
    break;

  case 1119:

/* Line 1455 of yacc.c  */
#line 6166 "parser.y"
    {
        (yyval) = cb_build_replacing_characters ((yyvsp[(3) - (4)]), (yyvsp[(4) - (4)]));
        inspect_keyword = 0;
  ;}
    break;

  case 1120:

/* Line 1455 of yacc.c  */
#line 6170 "parser.y"
    { (yyval) = (yyvsp[(2) - (2)]); ;}
    break;

  case 1121:

/* Line 1455 of yacc.c  */
#line 6174 "parser.y"
    { /* Nothing */ ;}
    break;

  case 1122:

/* Line 1455 of yacc.c  */
#line 6175 "parser.y"
    { inspect_keyword = 1; ;}
    break;

  case 1123:

/* Line 1455 of yacc.c  */
#line 6176 "parser.y"
    { inspect_keyword = 2; ;}
    break;

  case 1124:

/* Line 1455 of yacc.c  */
#line 6177 "parser.y"
    { inspect_keyword = 3; ;}
    break;

  case 1125:

/* Line 1455 of yacc.c  */
#line 6178 "parser.y"
    { inspect_keyword = 4; ;}
    break;

  case 1126:

/* Line 1455 of yacc.c  */
#line 6183 "parser.y"
    {
        switch (inspect_keyword) {
                case 1:
                        (yyval) = cb_build_replacing_all ((yyvsp[(1) - (4)]), (yyvsp[(3) - (4)]), (yyvsp[(4) - (4)]));
                        break;
                case 2:
                        (yyval) = cb_build_replacing_leading ((yyvsp[(1) - (4)]), (yyvsp[(3) - (4)]), (yyvsp[(4) - (4)]));
                        break;
                case 3:
                        (yyval) = cb_build_replacing_first ((yyvsp[(1) - (4)]), (yyvsp[(3) - (4)]), (yyvsp[(4) - (4)]));
                        break;
                case 4:
                        (yyval) = cb_build_replacing_trailing ((yyvsp[(1) - (4)]), (yyvsp[(3) - (4)]), (yyvsp[(4) - (4)]));
                        break;
                default:
                        cb_warning (_("INSPECT REPLACING missing a keyword, ALL assumed"));
                        (yyval) = cb_build_replacing_all ((yyvsp[(1) - (4)]), (yyvsp[(3) - (4)]), (yyvsp[(4) - (4)]));
                        break;
        }
  ;}
    break;

  case 1127:

/* Line 1455 of yacc.c  */
#line 6209 "parser.y"
    {
        (yyval) = cb_build_converting ((yyvsp[(3) - (6)]), (yyvsp[(5) - (6)]), (yyvsp[(6) - (6)]));
  ;}
    break;

  case 1128:

/* Line 1455 of yacc.c  */
#line 6217 "parser.y"
    { (yyval) = cb_build_inspect_region_start (); ;}
    break;

  case 1129:

/* Line 1455 of yacc.c  */
#line 6219 "parser.y"
    { (yyval) = cb_build_inspect_region ((yyvsp[(1) - (4)]), (yyvsp[(2) - (4)]), (yyvsp[(4) - (4)])); ;}
    break;

  case 1132:

/* Line 1455 of yacc.c  */
#line 6230 "parser.y"
    { BEGIN_STATEMENT ("MERGE", 0); ;}
    break;

  case 1134:

/* Line 1455 of yacc.c  */
#line 6240 "parser.y"
    { BEGIN_STATEMENT ("MOVE", 0); ;}
    break;

  case 1136:

/* Line 1455 of yacc.c  */
#line 6246 "parser.y"
    {
        cb_emit_move ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
  ;}
    break;

  case 1137:

/* Line 1455 of yacc.c  */
#line 6250 "parser.y"
    {
        cb_emit_move_corresponding ((yyvsp[(2) - (4)]), (yyvsp[(4) - (4)]));
  ;}
    break;

  case 1138:

/* Line 1455 of yacc.c  */
#line 6261 "parser.y"
    { BEGIN_STATEMENT ("MULTIPLY", TERM_MULTIPLY); rounded_used = 0;;}
    break;

  case 1140:

/* Line 1455 of yacc.c  */
#line 6268 "parser.y"
    {
   /*CIT*/
        void* e1 = (yyvsp[(1) - (4)]);
        if (CB_EXCEPTION_ENABLE (COB_EC_SIZE_OVERFLOW) ) {
            current_statement->handler_id = COB_EC_SIZE_OVERFLOW;
        } 
        if (current_statement->handler1 || current_statement->handler2) {
            e1 = cb_check_optimized_field_1((yyvsp[(1) - (4)]));
            cb_check_optimized_field((yyvsp[(3) - (4)]), 0);
        } else {
            cb_check_optimized_field((yyvsp[(3) - (4)]), 1);
        }
        cb_emit_arithmetic ((yyvsp[(3) - (4)]), '*', e1, !rounded_used);
  ;}
    break;

  case 1141:

/* Line 1455 of yacc.c  */
#line 6283 "parser.y"
    {
   /*CIT*/
        void* e1 = (yyvsp[(1) - (6)]);
        void* e3 = (yyvsp[(3) - (6)]);
        if (CB_EXCEPTION_ENABLE (COB_EC_SIZE_OVERFLOW) ) {
            current_statement->handler_id = COB_EC_SIZE_OVERFLOW;
        } 
        if (current_statement->handler1 || current_statement->handler2) {
            e1 = cb_check_optimized_field_1((yyvsp[(1) - (6)]));
            e3 = cb_check_optimized_field_1((yyvsp[(3) - (6)]));
            cb_check_optimized_field((yyvsp[(5) - (6)]), 0);
        } else {
            cb_check_optimized_field((yyvsp[(5) - (6)]), 1);
        }
        cb_emit_arithmetic ((yyvsp[(5) - (6)]), 0, cb_build_binary_op (e1, '*', e3), !rounded_used);
  ;}
    break;

  case 1142:

/* Line 1455 of yacc.c  */
#line 6302 "parser.y"
    { terminator_warning (TERM_MULTIPLY); ;}
    break;

  case 1143:

/* Line 1455 of yacc.c  */
#line 6303 "parser.y"
    { terminator_clear (TERM_MULTIPLY); ;}
    break;

  case 1144:

/* Line 1455 of yacc.c  */
#line 6312 "parser.y"
    { BEGIN_STATEMENT ("OPEN", 0); ;}
    break;

  case 1147:

/* Line 1455 of yacc.c  */
#line 6320 "parser.y"
    {
        cb_tree l;
        void *e2 = (yyvsp[(2) - (6)]);
        if ((yyvsp[(5) - (6)])) {
            e2 = (yyvsp[(5) - (6)]);
        }
        for (l = (yyvsp[(4) - (6)]); l; l = CB_CHAIN (l)) {
                if (CB_VALUE (l) != cb_error_node) {
                        BEGIN_IMPLICIT_STATEMENT ();
                        cb_emit_open (CB_VALUE (l), e2, (yyvsp[(3) - (6)]), (yyvsp[(6) - (6)]));
                }
        }
  ;}
    break;

  case 1148:

/* Line 1455 of yacc.c  */
#line 6336 "parser.y"
    { (yyval) = cb_int (COB_OPEN_INPUT); ;}
    break;

  case 1149:

/* Line 1455 of yacc.c  */
#line 6337 "parser.y"
    { (yyval) = cb_int (COB_OPEN_OUTPUT); ;}
    break;

  case 1150:

/* Line 1455 of yacc.c  */
#line 6338 "parser.y"
    { (yyval) = cb_int (COB_OPEN_I_O); ;}
    break;

  case 1151:

/* Line 1455 of yacc.c  */
#line 6339 "parser.y"
    { (yyval) = cb_int (COB_OPEN_EXTEND); ;}
    break;

  case 1152:

/* Line 1455 of yacc.c  */
#line 6343 "parser.y"
    { (yyval) = NULL; ;}
    break;

  case 1153:

/* Line 1455 of yacc.c  */
#line 6344 "parser.y"
    { (yyval) = (yyvsp[(3) - (3)]); ;}
    break;

  case 1154:

/* Line 1455 of yacc.c  */
#line 6349 "parser.y"
    { (yyval) = NULL; ;}
    break;

  case 1155:

/* Line 1455 of yacc.c  */
#line 6350 "parser.y"
    { (yyval) = cb_int0 ;/* ignored */ ;}
    break;

  case 1156:

/* Line 1455 of yacc.c  */
#line 6351 "parser.y"
    { (yyval) = cb_int (COB_LOCK_EXCLUSIVE);;}
    break;

  case 1157:

/* Line 1455 of yacc.c  */
#line 6355 "parser.y"
    { (yyval) = NULL; ;}
    break;

  case 1158:

/* Line 1455 of yacc.c  */
#line 6356 "parser.y"
    { (yyval) = cb_int (COB_OPEN_INPUT_REVERSED); ;}
    break;

  case 1159:

/* Line 1455 of yacc.c  */
#line 6364 "parser.y"
    { BEGIN_STATEMENT ("PERFORM", TERM_PERFORM); ;}
    break;

  case 1161:

/* Line 1455 of yacc.c  */
#line 6370 "parser.y"
    {
        cb_emit_perform ((yyvsp[(2) - (2)]), (yyvsp[(1) - (2)]));
  ;}
    break;

  case 1162:

/* Line 1455 of yacc.c  */
#line 6374 "parser.y"
    {
        perform_stack = cb_cons ((yyvsp[(1) - (1)]), perform_stack);
        check_unreached = 0;
  ;}
    break;

  case 1163:

/* Line 1455 of yacc.c  */
#line 6379 "parser.y"
    {
        perform_stack = CB_CHAIN (perform_stack);
        cb_emit_perform ((yyvsp[(1) - (4)]), (yyvsp[(3) - (4)]));
  ;}
    break;

  case 1164:

/* Line 1455 of yacc.c  */
#line 6384 "parser.y"
    {
        cb_emit_perform ((yyvsp[(1) - (2)]), NULL);
  ;}
    break;

  case 1165:

/* Line 1455 of yacc.c  */
#line 6391 "parser.y"
    { terminator_warning (TERM_PERFORM); ;}
    break;

  case 1166:

/* Line 1455 of yacc.c  */
#line 6392 "parser.y"
    { terminator_clear (TERM_PERFORM); ;}
    break;

  case 1167:

/* Line 1455 of yacc.c  */
#line 6397 "parser.y"
    {
        CB_REFERENCE ((yyvsp[(1) - (1)]))->length = cb_true; /* return from $1 */
        (yyval) = cb_build_pair ((yyvsp[(1) - (1)]), (yyvsp[(1) - (1)]));
  ;}
    break;

  case 1168:

/* Line 1455 of yacc.c  */
#line 6402 "parser.y"
    {
        CB_REFERENCE ((yyvsp[(3) - (3)]))->length = cb_true; /* return from $3 */
        (yyval) = cb_build_pair ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
  ;}
    break;

  case 1169:

/* Line 1455 of yacc.c  */
#line 6410 "parser.y"
    {
        (yyval) = cb_build_perform_once (NULL);
  ;}
    break;

  case 1170:

/* Line 1455 of yacc.c  */
#line 6414 "parser.y"
    {
    (yyval) = cb_build_perform_forever (NULL);
  ;}
    break;

  case 1171:

/* Line 1455 of yacc.c  */
#line 6418 "parser.y"
    {
        (yyval) = cb_build_perform_times ((yyvsp[(1) - (2)]));
  ;}
    break;

  case 1172:

/* Line 1455 of yacc.c  */
#line 6422 "parser.y"
    {
        if ((yyvsp[(3) - (3)]) == NULL)  {
            (yyval) = cb_build_perform_forever (NULL);
        } else {
            cb_tree varying;
    
            varying = cb_list_init (cb_build_perform_varying (NULL, NULL, NULL, (yyvsp[(3) - (3)])));
            (yyval) = cb_build_perform_until ((yyvsp[(1) - (3)]), varying);
        }
  ;}
    break;

  case 1173:

/* Line 1455 of yacc.c  */
#line 6433 "parser.y"
    {
        (yyval) = cb_build_perform_until ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
  ;}
    break;

  case 1174:

/* Line 1455 of yacc.c  */
#line 6439 "parser.y"
    { (yyval) = CB_BEFORE; ;}
    break;

  case 1175:

/* Line 1455 of yacc.c  */
#line 6440 "parser.y"
    { (yyval) = (yyvsp[(3) - (3)]); ;}
    break;

  case 1176:

/* Line 1455 of yacc.c  */
#line 6444 "parser.y"
    { (yyval) = (yyvsp[(1) - (1)]) ;  ;}
    break;

  case 1177:

/* Line 1455 of yacc.c  */
#line 6445 "parser.y"
    { (yyval) = NULL ;;}
    break;

  case 1178:

/* Line 1455 of yacc.c  */
#line 6449 "parser.y"
    { (yyval) = cb_list_init ((yyvsp[(1) - (1)])); ;}
    break;

  case 1179:

/* Line 1455 of yacc.c  */
#line 6451 "parser.y"
    { (yyval) = cb_list_add ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])); ;}
    break;

  case 1180:

/* Line 1455 of yacc.c  */
#line 6456 "parser.y"
    {
        (yyval) = cb_build_perform_varying ((yyvsp[(1) - (7)]), (yyvsp[(3) - (7)]), (yyvsp[(5) - (7)]), (yyvsp[(7) - (7)]));
  ;}
    break;

  case 1181:

/* Line 1455 of yacc.c  */
#line 6469 "parser.y"
    { 
       BEGIN_STATEMENT_1 ("PRAGMA", 0, 0); 
  ;}
    break;

  case 1182:

/* Line 1455 of yacc.c  */
#line 6473 "parser.y"
    {
       struct cb_literal *li;
       if ( (yyvsp[(3) - (4)]) && CB_LITERAL_P( (yyvsp[(3) - (4)])) ) {
         li = CB_LITERAL((yyvsp[(3) - (4)]));
         if ( (strcasecmp((char*)li->data, "DEBUGOFF") == 0) ) {
            cb_disable_runtime_check = 1;
         } else if ( (strcasecmp((char*)li->data, "DEBUGON") == 0) ) {
            cb_disable_runtime_check = 0;
         }         
       }
       cb_emit_pragma ((yyvsp[(3) - (4)]), (yyvsp[(4) - (4)]));
  ;}
    break;

  case 1187:

/* Line 1455 of yacc.c  */
#line 6498 "parser.y"
    { BEGIN_STATEMENT ("READ", TERM_READ); ;}
    break;

  case 1188:

/* Line 1455 of yacc.c  */
#line 6501 "parser.y"
    {
        if ((yyvsp[(3) - (11)]) != cb_error_node) {
                if ((yyvsp[(8) - (11)]) && (CB_FILE(cb_ref((yyvsp[(3) - (11)])))->lock_mode & COB_LOCK_AUTOMATIC)) {
                        cb_error (_("LOCK clause invalid with file LOCK AUTOMATIC"));
                } else if ((yyvsp[(9) - (11)]) &&
                      (CB_FILE(cb_ref((yyvsp[(3) - (11)])))->organization != COB_ORG_RELATIVE &&
                       CB_FILE(cb_ref((yyvsp[(3) - (11)])))->organization != COB_ORG_INDEXED) &&
                       (!cb_verify (cb_invalid_with_file_type, "KEY clause with wrong file type"))) {
                        cb_error (_("KEY clause invalid with this file type"));
                } else if (current_statement->handler_id == COB_EC_I_O_INVALID_KEY &&
                      (CB_FILE(cb_ref((yyvsp[(3) - (11)])))->organization != COB_ORG_RELATIVE &&
                       CB_FILE(cb_ref((yyvsp[(3) - (11)])))->organization != COB_ORG_INDEXED) && 
                       (!cb_verify (cb_invalid_with_file_type, "KEY clause with wrong file type"))) {
                       cb_error (_("INVALID KEY clause invalid with this file type"));
                } else {
                        cb_emit_read ((yyvsp[(3) - (11)]), (yyvsp[(4) - (11)]), (yyvsp[(7) - (11)]), (yyvsp[(9) - (11)]), (yyvsp[(6) - (11)]) == NULL ? (yyvsp[(8) - (11)]) : (yyvsp[(6) - (11)]) );
                }
        }
  ;}
    break;

  case 1189:

/* Line 1455 of yacc.c  */
#line 6523 "parser.y"
    { (yyval) = NULL; ;}
    break;

  case 1190:

/* Line 1455 of yacc.c  */
#line 6524 "parser.y"
    { (yyval) = (yyvsp[(2) - (2)]); ;}
    break;

  case 1191:

/* Line 1455 of yacc.c  */
#line 6528 "parser.y"
    { (yyval) = NULL; ;}
    break;

  case 1192:

/* Line 1455 of yacc.c  */
#line 6530 "parser.y"
    {
        (yyval) = cb_int3;
  ;}
    break;

  case 1193:

/* Line 1455 of yacc.c  */
#line 6534 "parser.y"
    {
        (yyval) = cb_int1;
  ;}
    break;

  case 1194:

/* Line 1455 of yacc.c  */
#line 6538 "parser.y"
    {
        (yyval) = cb_int2;
  ;}
    break;

  case 1195:

/* Line 1455 of yacc.c  */
#line 6542 "parser.y"
    {
        (yyval) = cb_int3;
  ;}
    break;

  case 1196:

/* Line 1455 of yacc.c  */
#line 6546 "parser.y"
    {
        (yyval) = cb_int4;
  ;}
    break;

  case 1197:

/* Line 1455 of yacc.c  */
#line 6550 "parser.y"
    {
        (yyval) = cb_int1;
  ;}
    break;

  case 1198:

/* Line 1455 of yacc.c  */
#line 6556 "parser.y"
    { (yyval) = NULL; ;}
    break;

  case 1199:

/* Line 1455 of yacc.c  */
#line 6558 "parser.y"
    {
        (yyval) = cb_int3;
  ;}
    break;

  case 1200:

/* Line 1455 of yacc.c  */
#line 6562 "parser.y"
    {
        (yyval) = cb_int1;
  ;}
    break;

  case 1201:

/* Line 1455 of yacc.c  */
#line 6566 "parser.y"
    {
        (yyval) = cb_int2;
  ;}
    break;

  case 1202:

/* Line 1455 of yacc.c  */
#line 6570 "parser.y"
    {
        (yyval) = cb_int3;
  ;}
    break;

  case 1203:

/* Line 1455 of yacc.c  */
#line 6574 "parser.y"
    {
        (yyval) = cb_int4;
  ;}
    break;

  case 1204:

/* Line 1455 of yacc.c  */
#line 6578 "parser.y"
    {
        (yyval) = cb_int1;
  ;}
    break;

  case 1205:

/* Line 1455 of yacc.c  */
#line 6584 "parser.y"
    { (yyval) = NULL; ;}
    break;

  case 1206:

/* Line 1455 of yacc.c  */
#line 6585 "parser.y"
    { (yyval) = (yyvsp[(3) - (3)]); ;}
    break;

  case 1210:

/* Line 1455 of yacc.c  */
#line 6594 "parser.y"
    { terminator_warning (TERM_READ); ;}
    break;

  case 1211:

/* Line 1455 of yacc.c  */
#line 6595 "parser.y"
    { terminator_clear (TERM_READ); ;}
    break;

  case 1212:

/* Line 1455 of yacc.c  */
#line 6605 "parser.y"
    {
        BEGIN_STATEMENT ("READY", 0);
        cb_emit_ready ();
  ;}
    break;

  case 1213:

/* Line 1455 of yacc.c  */
#line 6617 "parser.y"
    {
        BEGIN_STATEMENT ("RESET", 0);
        cb_emit_reset ();
  ;}
    break;

  case 1214:

/* Line 1455 of yacc.c  */
#line 6629 "parser.y"
    { BEGIN_STATEMENT ("RELEASE", 0); ;}
    break;

  case 1215:

/* Line 1455 of yacc.c  */
#line 6631 "parser.y"
    {
    if ((yyvsp[(3) - (4)]) != cb_error_node) {
        cb_emit_release ((yyvsp[(3) - (4)]), (yyvsp[(4) - (4)]));
  }
  ;}
    break;

  case 1216:

/* Line 1455 of yacc.c  */
#line 6644 "parser.y"
    { BEGIN_STATEMENT ("RETURN", TERM_RETURN); ;}
    break;

  case 1217:

/* Line 1455 of yacc.c  */
#line 6647 "parser.y"
    {
    if ((yyvsp[(3) - (7)]) != cb_error_node) {
        cb_emit_return ((yyvsp[(3) - (7)]), (yyvsp[(5) - (7)]));
  }
  ;}
    break;

  case 1218:

/* Line 1455 of yacc.c  */
#line 6655 "parser.y"
    { terminator_warning (TERM_RETURN); ;}
    break;

  case 1219:

/* Line 1455 of yacc.c  */
#line 6656 "parser.y"
    { terminator_clear (TERM_RETURN); ;}
    break;

  case 1220:

/* Line 1455 of yacc.c  */
#line 6665 "parser.y"
    { BEGIN_STATEMENT ("REWRITE", TERM_REWRITE); ;}
    break;

  case 1221:

/* Line 1455 of yacc.c  */
#line 6668 "parser.y"
    {
        if ((yyvsp[(3) - (7)]) != cb_error_node) {
                cb_emit_rewrite ((yyvsp[(3) - (7)]), (yyvsp[(4) - (7)]), (yyvsp[(5) - (7)]));
        }
       if (cb_flag_fdclear){
           current_statement->final = cb_list_add(current_statement->final,
              cb_build_initialize_vars(cb_list_init ((yyvsp[(3) - (7)])),NULL,NULL,NULL,cb_flag_initalize_to_value || cb_flag_initialize_to_value ?  cb_true: NULL, 1));
        }
 
  ;}
    break;

  case 1222:

/* Line 1455 of yacc.c  */
#line 6681 "parser.y"
    { (yyval) = NULL; ;}
    break;

  case 1223:

/* Line 1455 of yacc.c  */
#line 6683 "parser.y"
    {
        (yyval) = cb_int1;
  ;}
    break;

  case 1224:

/* Line 1455 of yacc.c  */
#line 6687 "parser.y"
    {
        (yyval) = cb_int2;
  ;}
    break;

  case 1225:

/* Line 1455 of yacc.c  */
#line 6693 "parser.y"
    { terminator_warning (TERM_REWRITE); ;}
    break;

  case 1226:

/* Line 1455 of yacc.c  */
#line 6694 "parser.y"
    { terminator_clear (TERM_REWRITE); ;}
    break;

  case 1227:

/* Line 1455 of yacc.c  */
#line 6704 "parser.y"
    {
        BEGIN_STATEMENT ("ROLLBACK", 0);
        cb_emit_rollback ();
  ;}
    break;

  case 1228:

/* Line 1455 of yacc.c  */
#line 6716 "parser.y"
    { BEGIN_STATEMENT ("SEARCH", TERM_SEARCH); ;}
    break;

  case 1230:

/* Line 1455 of yacc.c  */
#line 6723 "parser.y"
    {
        cb_emit_search ((yyvsp[(1) - (4)]), (yyvsp[(2) - (4)]), (yyvsp[(3) - (4)]), (yyvsp[(4) - (4)]));
  ;}
    break;

  case 1231:

/* Line 1455 of yacc.c  */
#line 6727 "parser.y"
    {
    check_unreached = 0;
  ;}
    break;

  case 1232:

/* Line 1455 of yacc.c  */
#line 6731 "parser.y"
    {
    cb_emit_search_all ((yyvsp[(2) - (7)]), (yyvsp[(3) - (7)]), (yyvsp[(5) - (7)]), (yyvsp[(7) - (7)]));
  ;}
    break;

  case 1233:

/* Line 1455 of yacc.c  */
#line 6737 "parser.y"
    { (yyval) = NULL; ;}
    break;

  case 1234:

/* Line 1455 of yacc.c  */
#line 6738 "parser.y"
    { (yyval) = (yyvsp[(2) - (2)]); ;}
    break;

  case 1235:

/* Line 1455 of yacc.c  */
#line 6742 "parser.y"
    { (yyval) = NULL; ;}
    break;

  case 1236:

/* Line 1455 of yacc.c  */
#line 6744 "parser.y"
    {
    check_unreached = 0;
  ;}
    break;

  case 1237:

/* Line 1455 of yacc.c  */
#line 6748 "parser.y"
    {
    (yyval) = (yyvsp[(4) - (4)]);
  ;}
    break;

  case 1238:

/* Line 1455 of yacc.c  */
#line 6754 "parser.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 1239:

/* Line 1455 of yacc.c  */
#line 6755 "parser.y"
    { (yyval) = (yyvsp[(1) - (2)]); CB_IF ((yyvsp[(1) - (2)]))->stmt2 = (yyvsp[(2) - (2)]); ;}
    break;

  case 1240:

/* Line 1455 of yacc.c  */
#line 6760 "parser.y"
    {
    check_unreached = 0;
  ;}
    break;

  case 1241:

/* Line 1455 of yacc.c  */
#line 6764 "parser.y"
    {
    (yyval) = cb_build_if ((yyvsp[(2) - (4)]), (yyvsp[(4) - (4)]), NULL);
  ;}
    break;

  case 1242:

/* Line 1455 of yacc.c  */
#line 6771 "parser.y"
    { terminator_warning (TERM_SEARCH); ;}
    break;

  case 1243:

/* Line 1455 of yacc.c  */
#line 6772 "parser.y"
    { terminator_clear (TERM_SEARCH); ;}
    break;

  case 1244:

/* Line 1455 of yacc.c  */
#line 6781 "parser.y"
    { BEGIN_STATEMENT ("SET", 0); ;}
    break;

  case 1251:

/* Line 1455 of yacc.c  */
#line 6797 "parser.y"
    {
        cb_emit_setenv ((yyvsp[(2) - (4)]), (yyvsp[(4) - (4)]));
  ;}
    break;

  case 1252:

/* Line 1455 of yacc.c  */
#line 6806 "parser.y"
    {
        cb_emit_set_to ((yyvsp[(1) - (4)]), cb_build_ppointer ((yyvsp[(4) - (4)])));
  ;}
    break;

  case 1253:

/* Line 1455 of yacc.c  */
#line 6810 "parser.y"
    {
        cb_emit_set_to ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
  ;}
    break;

  case 1254:

/* Line 1455 of yacc.c  */
#line 6819 "parser.y"
    {
        cb_emit_set_up_down ((yyvsp[(1) - (4)]), (yyvsp[(2) - (4)]), (yyvsp[(4) - (4)]));
  ;}
    break;

  case 1255:

/* Line 1455 of yacc.c  */
#line 6825 "parser.y"
    { (yyval) = cb_int0; ;}
    break;

  case 1256:

/* Line 1455 of yacc.c  */
#line 6826 "parser.y"
    { (yyval) = cb_int1; ;}
    break;

  case 1259:

/* Line 1455 of yacc.c  */
#line 6838 "parser.y"
    {
        cb_emit_set_on_off ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
  ;}
    break;

  case 1262:

/* Line 1455 of yacc.c  */
#line 6852 "parser.y"
    {
        cb_emit_set_true ((yyvsp[(1) - (3)]));
  ;}
    break;

  case 1263:

/* Line 1455 of yacc.c  */
#line 6856 "parser.y"
    {
        cb_emit_set_false ((yyvsp[(1) - (3)]));
  ;}
    break;

  case 1264:

/* Line 1455 of yacc.c  */
#line 6867 "parser.y"
    { BEGIN_STATEMENT ("SORT", 0); ;}
    break;

  case 1266:

/* Line 1455 of yacc.c  */
#line 6873 "parser.y"
    {
        cb_tree x;
        if (!(CB_FILE_P (cb_ref ((yyvsp[(1) - (4)])))))
            x = (yyvsp[(1) - (4)])  ;
        else 
            x = cb_build_file_reference(CB_NAME((yyvsp[(1) - (4)])));          
        cb_emit_sort_init (x, (yyvsp[(2) - (4)]), (yyvsp[(4) - (4)]));
        if (CB_FILE_P (cb_ref (x)) && (yyvsp[(2) - (4)]) == NULL) {
                cb_error (_("File sort requires KEY phrase"));
        }
    /* used in sort_input/sort_output */
    save_tree_1 = x;
    sort_input_proc = 0;
  ;}
    break;

  case 1267:

/* Line 1455 of yacc.c  */
#line 6888 "parser.y"
    {
        cb_emit_sort_finish (save_tree_1);
  ;}
    break;

  case 1268:

/* Line 1455 of yacc.c  */
#line 6895 "parser.y"
    {
        (yyval) = NULL;
  ;}
    break;

  case 1269:

/* Line 1455 of yacc.c  */
#line 6900 "parser.y"
    {
        cb_tree l;
        void *e5 =  (yyvsp[(5) - (5)]);
        if (e5 == NULL) {
            e5 = cb_list_init (NULL);
        }
        for (l = e5; l; l = CB_CHAIN (l)) {
                CB_PURPOSE (l) = (yyvsp[(3) - (5)]);
        }
        (yyval) = cb_list_append ((yyvsp[(1) - (5)]), e5);
  ;}
    break;

  case 1270:

/* Line 1455 of yacc.c  */
#line 6914 "parser.y"
    { (yyval) = NULL; ;}
    break;

  case 1271:

/* Line 1455 of yacc.c  */
#line 6915 "parser.y"
    { (yyval) = cb_list_add ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])); ;}
    break;

  case 1273:

/* Line 1455 of yacc.c  */
#line 6919 "parser.y"
    { /* nothing */ ;}
    break;

  case 1274:

/* Line 1455 of yacc.c  */
#line 6923 "parser.y"
    { (yyval) = cb_null; ;}
    break;

  case 1275:

/* Line 1455 of yacc.c  */
#line 6924 "parser.y"
    { (yyval) = cb_ref ((yyvsp[(3) - (3)])); ;}
    break;

  case 1276:

/* Line 1455 of yacc.c  */
#line 6929 "parser.y"
    {
    if (CB_FILE_P (cb_ref (save_tree_1))) {
                cb_error (_("File sort requires USING or INPUT PROCEDURE"));
        }
  ;}
    break;

  case 1277:

/* Line 1455 of yacc.c  */
#line 6935 "parser.y"
    {
    if (!CB_FILE_P (cb_ref (save_tree_1))) {
                cb_error (_("USING invalid with table SORT"));
        } else {
        cb_emit_sort_using (save_tree_1, (yyvsp[(2) - (2)]));
    }
  ;}
    break;

  case 1278:

/* Line 1455 of yacc.c  */
#line 6943 "parser.y"
    {
    if (!CB_FILE_P (cb_ref (save_tree_1))) {
                cb_error (_("INPUT PROCEDURE invalid with table SORT"));
        } else {
        cb_emit_sort_input (save_tree_1, (yyvsp[(4) - (4)]));
        sort_input_proc = 1;
    }
  ;}
    break;

  case 1279:

/* Line 1455 of yacc.c  */
#line 6955 "parser.y"
    {
    if (CB_FILE_P (cb_ref (save_tree_1))) {
                cb_error (_("File sort requires GIVING or OUTPUT PROCEDURE"));
        }
  ;}
    break;

  case 1280:

/* Line 1455 of yacc.c  */
#line 6961 "parser.y"
    {
    if (!CB_FILE_P (cb_ref (save_tree_1))) {
                cb_error (_("GIVING invalid with table SORT"));
        } else {
        cb_emit_sort_giving (save_tree_1, (yyvsp[(2) - (2)]), sort_input_proc);
    }
  ;}
    break;

  case 1281:

/* Line 1455 of yacc.c  */
#line 6969 "parser.y"
    {
    if (!CB_FILE_P (cb_ref (save_tree_1))) {
                cb_error (_("OUTPUT PROCEDURE invalid with table SORT"));
        } else {
        cb_emit_sort_output (save_tree_1, (yyvsp[(4) - (4)]));
    }
  ;}
    break;

  case 1282:

/* Line 1455 of yacc.c  */
#line 6984 "parser.y"
    { BEGIN_STATEMENT ("START", TERM_START); ;}
    break;

  case 1283:

/* Line 1455 of yacc.c  */
#line 6985 "parser.y"
    { (yyval) = cb_int (COB_EQ); start_cond = (yyval);;}
    break;

  case 1284:

/* Line 1455 of yacc.c  */
#line 6988 "parser.y"
    {
        
        if (CB_FILE_P (cb_ref ((yyvsp[(3) - (8)])))) {
                if (CB_FILE (cb_ref ((yyvsp[(3) - (8)])))->organization != COB_ORG_INDEXED &&
                     CB_FILE (cb_ref ((yyvsp[(3) - (8)])))->organization != COB_ORG_RELATIVE) {
                        cb_error (_("START not allowed on SEQUENTIAL files"));
                        (yyval) = cb_error_node;
                } else {
                        cb_emit_start ((yyvsp[(3) - (8)]), start_cond, (yyvsp[(6) - (8)]));
                }
        } else {
                if ((yyvsp[(3) - (8)]) != cb_error_node) {
                    cb_error_x ((yyvsp[(3) - (8)]), _("'%s' is not a file name"), CB_NAME ((yyvsp[(3) - (8)])));
                }
                (yyval) = cb_error_node;
        }
  ;}
    break;

  case 1285:

/* Line 1455 of yacc.c  */
#line 7008 "parser.y"
    { start_cond = cb_int (COB_EQ); (yyval) = NULL; ;}
    break;

  case 1286:

/* Line 1455 of yacc.c  */
#line 7009 "parser.y"
    { start_cond = (yyvsp[(3) - (4)]); (yyval) = (yyvsp[(4) - (4)]); ;}
    break;

  case 1287:

/* Line 1455 of yacc.c  */
#line 7013 "parser.y"
    { (yyval) = cb_int (((yyvsp[(1) - (2)]) == cb_int1) ? COB_NE : COB_EQ); ;}
    break;

  case 1288:

/* Line 1455 of yacc.c  */
#line 7014 "parser.y"
    { (yyval) = cb_int (((yyvsp[(1) - (2)]) == cb_int1) ? COB_LE : COB_GT); ;}
    break;

  case 1289:

/* Line 1455 of yacc.c  */
#line 7015 "parser.y"
    { (yyval) = cb_int (((yyvsp[(1) - (2)]) == cb_int1) ? COB_GE : COB_LT); ;}
    break;

  case 1290:

/* Line 1455 of yacc.c  */
#line 7016 "parser.y"
    { (yyval) = cb_int (((yyvsp[(1) - (2)]) == cb_int1) ? COB_LT : COB_GE); ;}
    break;

  case 1291:

/* Line 1455 of yacc.c  */
#line 7017 "parser.y"
    { (yyval) = cb_int (((yyvsp[(1) - (2)]) == cb_int1) ? COB_GT : COB_LE); ;}
    break;

  case 1292:

/* Line 1455 of yacc.c  */
#line 7021 "parser.y"
    { terminator_warning (TERM_START); ;}
    break;

  case 1293:

/* Line 1455 of yacc.c  */
#line 7022 "parser.y"
    { terminator_clear (TERM_START); ;}
    break;

  case 1294:

/* Line 1455 of yacc.c  */
#line 7031 "parser.y"
    { BEGIN_STATEMENT ("STOP", 0); ;}
    break;

  case 1296:

/* Line 1455 of yacc.c  */
#line 7038 "parser.y"
    {
        cb_emit_stop_run ((yyvsp[(2) - (2)]));
  ;}
    break;

  case 1297:

/* Line 1455 of yacc.c  */
#line 7042 "parser.y"
    {
        cb_verify (cb_stop_literal_statement, "STOP literal");
  ;}
    break;

  case 1298:

/* Line 1455 of yacc.c  */
#line 7048 "parser.y"
    {
        cb_emit_stop_abend ((yyvsp[(2) - (3)])); 
  ;}
    break;

  case 1299:

/* Line 1455 of yacc.c  */
#line 7055 "parser.y"
    { (yyval) = current_program->cb_return_code; ;}
    break;

  case 1300:

/* Line 1455 of yacc.c  */
#line 7056 "parser.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 1301:

/* Line 1455 of yacc.c  */
#line 7057 "parser.y"
    { (yyval) = (yyvsp[(2) - (2)]); ;}
    break;

  case 1302:

/* Line 1455 of yacc.c  */
#line 7058 "parser.y"
    { (yyval) = (yyvsp[(2) - (2)]); ;}
    break;

  case 1303:

/* Line 1455 of yacc.c  */
#line 7067 "parser.y"
    { BEGIN_STATEMENT ("STRING", TERM_STRING); ;}
    break;

  case 1304:

/* Line 1455 of yacc.c  */
#line 7070 "parser.y"
    {
        cb_emit_string ((yyvsp[(3) - (8)]), (yyvsp[(5) - (8)]), (yyvsp[(6) - (8)]));
  ;}
    break;

  case 1305:

/* Line 1455 of yacc.c  */
#line 7076 "parser.y"
    { (yyval) = cb_list_init ((yyvsp[(1) - (1)])); ;}
    break;

  case 1306:

/* Line 1455 of yacc.c  */
#line 7077 "parser.y"
    { (yyval) = cb_list_add ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])); ;}
    break;

  case 1307:

/* Line 1455 of yacc.c  */
#line 7081 "parser.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 1308:

/* Line 1455 of yacc.c  */
#line 7082 "parser.y"
    { (yyval) = cb_build_pair (cb_int0, NULL); ;}
    break;

  case 1309:

/* Line 1455 of yacc.c  */
#line 7083 "parser.y"
    { (yyval) = cb_build_pair ((yyvsp[(3) - (3)]), NULL); ;}
    break;

  case 1310:

/* Line 1455 of yacc.c  */
#line 7087 "parser.y"
    { (yyval) = cb_int0; ;}
    break;

  case 1311:

/* Line 1455 of yacc.c  */
#line 7088 "parser.y"
    { (yyval) = (yyvsp[(3) - (3)]); ;}
    break;

  case 1312:

/* Line 1455 of yacc.c  */
#line 7092 "parser.y"
    { terminator_warning (TERM_STRING); ;}
    break;

  case 1313:

/* Line 1455 of yacc.c  */
#line 7093 "parser.y"
    { terminator_clear (TERM_STRING); ;}
    break;

  case 1314:

/* Line 1455 of yacc.c  */
#line 7102 "parser.y"
    { BEGIN_STATEMENT ("SUBTRACT", TERM_SUBTRACT); rounded_used = 0;;}
    break;

  case 1316:

/* Line 1455 of yacc.c  */
#line 7109 "parser.y"
    {
   /*CIT*/  
        if (CB_EXCEPTION_ENABLE (COB_EC_SIZE_OVERFLOW) ) {
            current_statement->handler_id = COB_EC_SIZE_OVERFLOW;
        } 
        if (current_statement->handler1 || current_statement->handler2) {
            cb_check_optimized_field((yyvsp[(3) - (4)]), 0);
            cb_check_optimized_field((yyvsp[(1) - (4)]), 0);
        } else {
            cb_check_optimized_field((yyvsp[(3) - (4)]), 1);
        }
        cb_emit_arithmetic ((yyvsp[(3) - (4)]), '-', cb_build_binary_list ((yyvsp[(1) - (4)]), '+'), !rounded_used);
  ;}
    break;

  case 1317:

/* Line 1455 of yacc.c  */
#line 7123 "parser.y"
    {
   /*CIT*/
        void * e3 = (yyvsp[(3) - (6)]);
        if (CB_EXCEPTION_ENABLE (COB_EC_SIZE_OVERFLOW) ) {
            current_statement->handler_id = COB_EC_SIZE_OVERFLOW;
        } 
        if (current_statement->handler1 || current_statement->handler2) {
            cb_check_optimized_field((yyvsp[(5) - (6)]), 0);
            e3 = cb_check_optimized_field_1(e3);
            cb_check_optimized_field((yyvsp[(1) - (6)]), 0);
        } else {
            cb_check_optimized_field((yyvsp[(5) - (6)]), 1);
        }
        cb_emit_arithmetic ((yyvsp[(5) - (6)]), 0, cb_build_binary_list (cb_cons (e3, (yyvsp[(1) - (6)])), '-'), !rounded_used );
  ;}
    break;

  case 1318:

/* Line 1455 of yacc.c  */
#line 7139 "parser.y"
    {
        void *e2 = (yyvsp[(2) - (6)]);
        void *e4 = (yyvsp[(4) - (6)]);
        if (CB_EXCEPTION_ENABLE (COB_EC_SIZE_OVERFLOW) ) {
            current_statement->handler_id = COB_EC_SIZE_OVERFLOW;
        } 
        if (current_statement->handler1 || current_statement->handler2 || (yyvsp[(5) - (6)]) != cb_int0) {
            e2 = cb_check_optimized_field_1(e2);
            e4 = cb_check_optimized_field_1(e4);
        }
        cb_emit_corresponding (cb_build_sub, e4, e2, (yyvsp[(5) - (6)]));
  ;}
    break;

  case 1319:

/* Line 1455 of yacc.c  */
#line 7154 "parser.y"
    { terminator_warning (TERM_SUBTRACT); ;}
    break;

  case 1320:

/* Line 1455 of yacc.c  */
#line 7155 "parser.y"
    { terminator_clear (TERM_SUBTRACT); ;}
    break;

  case 1321:

/* Line 1455 of yacc.c  */
#line 7165 "parser.y"
    {
    BEGIN_STATEMENT ("SUPPRESS", 0);
    PENDING("SUPPRESS");
  ;}
    break;

  case 1324:

/* Line 1455 of yacc.c  */
#line 7180 "parser.y"
    { BEGIN_STATEMENT ("TERMINATE", 0); ;}
    break;

  case 1325:

/* Line 1455 of yacc.c  */
#line 7182 "parser.y"
    {
    PENDING("TERMINATE");
  ;}
    break;

  case 1326:

/* Line 1455 of yacc.c  */
#line 7193 "parser.y"
    { BEGIN_STATEMENT ("TRANSFORM", 0); ;}
    break;

  case 1327:

/* Line 1455 of yacc.c  */
#line 7195 "parser.y"
    {
    cb_tree     x;

    x = cb_build_converting ((yyvsp[(5) - (7)]), (yyvsp[(7) - (7)]), cb_build_inspect_region_start ());
    cb_emit_inspect ((yyvsp[(3) - (7)]), x, cb_int0, 2);
  ;}
    break;

  case 1328:

/* Line 1455 of yacc.c  */
#line 7209 "parser.y"
    { BEGIN_STATEMENT ("UNLOCK", 0); ;}
    break;

  case 1329:

/* Line 1455 of yacc.c  */
#line 7211 "parser.y"
    {
        if ((yyvsp[(3) - (4)]) != cb_error_node) {
                cb_emit_unlock ((yyvsp[(3) - (4)]));
        }
  ;}
    break;

  case 1333:

/* Line 1455 of yacc.c  */
#line 7230 "parser.y"
    { BEGIN_STATEMENT ("UNSTRING", TERM_UNSTRING); ;}
    break;

  case 1334:

/* Line 1455 of yacc.c  */
#line 7234 "parser.y"
    {
        cb_emit_unstring ((yyvsp[(3) - (9)]), (yyvsp[(4) - (9)]), (yyvsp[(5) - (9)]), (yyvsp[(6) - (9)]), (yyvsp[(7) - (9)]));
  ;}
    break;

  case 1335:

/* Line 1455 of yacc.c  */
#line 7240 "parser.y"
    { (yyval) = NULL; ;}
    break;

  case 1336:

/* Line 1455 of yacc.c  */
#line 7242 "parser.y"
    { (yyval) = (yyvsp[(3) - (3)]); ;}
    break;

  case 1337:

/* Line 1455 of yacc.c  */
#line 7246 "parser.y"
    { (yyval) = cb_list_init ((yyvsp[(1) - (1)])); ;}
    break;

  case 1338:

/* Line 1455 of yacc.c  */
#line 7248 "parser.y"
    { (yyval) = cb_list_add ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])); ;}
    break;

  case 1339:

/* Line 1455 of yacc.c  */
#line 7253 "parser.y"
    {
        (yyval) = cb_build_unstring_delimited ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]));
  ;}
    break;

  case 1340:

/* Line 1455 of yacc.c  */
#line 7259 "parser.y"
    { (yyval) = cb_list_init ((yyvsp[(2) - (2)])); ;}
    break;

  case 1341:

/* Line 1455 of yacc.c  */
#line 7261 "parser.y"
    { (yyval) = cb_list_add ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])); ;}
    break;

  case 1342:

/* Line 1455 of yacc.c  */
#line 7266 "parser.y"
    {
        (yyval) = cb_build_unstring_into ((yyvsp[(1) - (3)]), (yyvsp[(2) - (3)]), (yyvsp[(3) - (3)]));
  ;}
    break;

  case 1343:

/* Line 1455 of yacc.c  */
#line 7272 "parser.y"
    { (yyval) = NULL; ;}
    break;

  case 1344:

/* Line 1455 of yacc.c  */
#line 7273 "parser.y"
    { (yyval) = (yyvsp[(3) - (3)]); ;}
    break;

  case 1345:

/* Line 1455 of yacc.c  */
#line 7277 "parser.y"
    { (yyval) = NULL; ;}
    break;

  case 1346:

/* Line 1455 of yacc.c  */
#line 7278 "parser.y"
    { (yyval) = (yyvsp[(3) - (3)]); ;}
    break;

  case 1347:

/* Line 1455 of yacc.c  */
#line 7282 "parser.y"
    { (yyval) = NULL; ;}
    break;

  case 1348:

/* Line 1455 of yacc.c  */
#line 7283 "parser.y"
    { (yyval) = (yyvsp[(3) - (3)]); ;}
    break;

  case 1349:

/* Line 1455 of yacc.c  */
#line 7287 "parser.y"
    { terminator_warning (TERM_UNSTRING); ;}
    break;

  case 1350:

/* Line 1455 of yacc.c  */
#line 7288 "parser.y"
    { terminator_clear (TERM_UNSTRING); ;}
    break;

  case 1354:

/* Line 1455 of yacc.c  */
#line 7306 "parser.y"
    {
        if (!in_declaratives) {
                cb_error (_("USE statement must be within DECLARATIVES"));
        } else if (!current_section) {
                cb_error (_("SECTION header missing before USE statement"));
        } else {
                current_section->need_begin = 1;
                current_section->need_return = 1;
                current_section->is_exception = 1;
                CB_EXCEPTION_ENABLE (COB_EC_I_O) = 1;
                if (use_global_ind) {
                    current_section->is_global = 1;
                    current_program->global_list =
                    cb_list_add (current_program->global_list,
                         CB_TREE (current_section));
                }
        }
  ;}
    break;

  case 1355:

/* Line 1455 of yacc.c  */
#line 7328 "parser.y"
    {
    use_global_ind = 0;
  ;}
    break;

  case 1356:

/* Line 1455 of yacc.c  */
#line 7332 "parser.y"
    {
    use_global_ind = 1;
    current_program->flag_global_use = 1;
  ;}
    break;

  case 1357:

/* Line 1455 of yacc.c  */
#line 7340 "parser.y"
    {
        cb_tree l;

        for (l = (yyvsp[(1) - (1)]); l; l = CB_CHAIN (l)) {
                if (CB_VALUE (l) != cb_error_node) {
            setup_use_file (CB_FILE (cb_ref (CB_VALUE (l))));
        }
    }
  ;}
    break;

  case 1358:

/* Line 1455 of yacc.c  */
#line 7350 "parser.y"
    {
    current_program->global_handler[COB_OPEN_INPUT].handler_label = current_section;
    current_program->global_handler[COB_OPEN_INPUT].handler_prog = current_program;
  ;}
    break;

  case 1359:

/* Line 1455 of yacc.c  */
#line 7355 "parser.y"
    {
    current_program->global_handler[COB_OPEN_OUTPUT].handler_label = current_section;
    current_program->global_handler[COB_OPEN_OUTPUT].handler_prog = current_program;
  ;}
    break;

  case 1360:

/* Line 1455 of yacc.c  */
#line 7360 "parser.y"
    {
    current_program->global_handler[COB_OPEN_I_O].handler_label = current_section;
    current_program->global_handler[COB_OPEN_I_O].handler_prog = current_program;
  ;}
    break;

  case 1361:

/* Line 1455 of yacc.c  */
#line 7365 "parser.y"
    {
    current_program->global_handler[COB_OPEN_EXTEND].handler_label = current_section;
    current_program->global_handler[COB_OPEN_EXTEND].handler_prog = current_program;
  ;}
    break;

  case 1374:

/* Line 1455 of yacc.c  */
#line 7397 "parser.y"
    {
        PENDING ("USE FOR DEBUGGING");
  ;}
    break;

  case 1377:

/* Line 1455 of yacc.c  */
#line 7409 "parser.y"
    {
    PENDING ("USE BEFORE REPORTING");
  ;}
    break;

  case 1378:

/* Line 1455 of yacc.c  */
#line 7420 "parser.y"
    { BEGIN_STATEMENT ("WRITE", TERM_WRITE); ;}
    break;

  case 1379:

/* Line 1455 of yacc.c  */
#line 7423 "parser.y"
    {
        if (((yyvsp[(3) - (8)]) != cb_error_node) && (yyvsp[(3) - (8)])) {
                cb_emit_write ((yyvsp[(3) - (8)]), (yyvsp[(4) - (8)]), (yyvsp[(6) - (8)]), (yyvsp[(5) - (8)]));
          if (cb_flag_fdclear){
              current_statement->final = cb_list_add(current_statement->final,
                 cb_build_initialize_vars(cb_list_init ((yyvsp[(3) - (8)])),NULL,NULL,NULL,cb_flag_initalize_to_value || cb_flag_initialize_to_value ?  cb_true: NULL, 1));
           }
      }
  ;}
    break;

  case 1380:

/* Line 1455 of yacc.c  */
#line 7435 "parser.y"
    { (yyval) = NULL; ;}
    break;

  case 1381:

/* Line 1455 of yacc.c  */
#line 7436 "parser.y"
    { (yyval) = (yyvsp[(2) - (2)]); ;}
    break;

  case 1382:

/* Line 1455 of yacc.c  */
#line 7441 "parser.y"
    {
        (yyval) = cb_int0;
  ;}
    break;

  case 1383:

/* Line 1455 of yacc.c  */
#line 7445 "parser.y"
    {
        (yyval) = cb_build_write_advancing_lines ((yyvsp[(1) - (4)]), (yyvsp[(3) - (4)]));
  ;}
    break;

  case 1384:

/* Line 1455 of yacc.c  */
#line 7449 "parser.y"
    {
        (yyval) = cb_build_write_advancing_mnemonic ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
  ;}
    break;

  case 1385:

/* Line 1455 of yacc.c  */
#line 7453 "parser.y"
    {
        (yyval) = cb_build_write_advancing_page ((yyvsp[(1) - (3)]));
  ;}
    break;

  case 1386:

/* Line 1455 of yacc.c  */
#line 7459 "parser.y"
    { (yyval) = CB_BEFORE; ;}
    break;

  case 1387:

/* Line 1455 of yacc.c  */
#line 7460 "parser.y"
    { (yyval) = CB_AFTER; ;}
    break;

  case 1391:

/* Line 1455 of yacc.c  */
#line 7469 "parser.y"
    { terminator_warning (TERM_WRITE); ;}
    break;

  case 1392:

/* Line 1455 of yacc.c  */
#line 7470 "parser.y"
    { terminator_clear (TERM_WRITE); ;}
    break;

  case 1393:

/* Line 1455 of yacc.c  */
#line 7485 "parser.y"
    {
        current_statement->handler_id = COB_EC_IMP_ACCEPT;
  ;}
    break;

  case 1394:

/* Line 1455 of yacc.c  */
#line 7493 "parser.y"
    {
        current_statement->handler_id = COB_EC_IMP_DISPLAY;
  ;}
    break;

  case 1395:

/* Line 1455 of yacc.c  */
#line 7501 "parser.y"
    {
        current_statement->handler_id = COB_EC_XML;
  ;}
    break;

  case 1397:

/* Line 1455 of yacc.c  */
#line 7508 "parser.y"
    {
    save_display_column = display_column;
    save_display_line = display_line;
    save_display_fgc = display_fgc;
    save_display_bgc = display_bgc;
    save_scroll = scroll;
    save_dispattrs = dispattrs;
    save_dispprompt = dispprompt; 
    check_unreached = 0;
  ;}
    break;

  case 1398:

/* Line 1455 of yacc.c  */
#line 7520 "parser.y"
    {
    current_statement->handler1 = (yyvsp[(4) - (4)]);
    display_column = save_display_column;
    display_line = save_display_line;
    display_fgc = save_display_fgc;
    display_bgc = save_display_bgc;
    scroll = save_scroll;
    dispattrs = save_dispattrs; 
    dispprompt = save_dispprompt;
  ;}
    break;

  case 1400:

/* Line 1455 of yacc.c  */
#line 7534 "parser.y"
    {
    save_display_column = display_column;
    save_display_line = display_line;
    save_display_fgc = display_fgc;
    save_display_bgc = display_bgc;
    save_scroll = scroll;
    save_dispattrs = dispattrs;
    save_dispprompt = dispprompt; 
    check_unreached = 0;
  ;}
    break;

  case 1401:

/* Line 1455 of yacc.c  */
#line 7545 "parser.y"
    {
    current_statement->handler2 = (yyvsp[(3) - (3)]);
    display_column = save_display_column;
    display_line = save_display_line;
    display_fgc = save_display_fgc;
    display_bgc = save_display_bgc;
    scroll = save_scroll;
    dispattrs = save_dispattrs; 
    dispprompt = save_dispprompt;
  ;}
    break;

  case 1403:

/* Line 1455 of yacc.c  */
#line 7559 "parser.y"
    {
    acc_exception_field = (yyvsp[(1) - (1)]);
  ;}
    break;

  case 1404:

/* Line 1455 of yacc.c  */
#line 7570 "parser.y"
    {
        current_statement->handler_id = COB_EC_SIZE;
  ;}
    break;

  case 1406:

/* Line 1455 of yacc.c  */
#line 7577 "parser.y"
    {
    check_unreached = 0;
  ;}
    break;

  case 1407:

/* Line 1455 of yacc.c  */
#line 7581 "parser.y"
    {
    current_statement->handler1 = (yyvsp[(3) - (3)]);
  ;}
    break;

  case 1409:

/* Line 1455 of yacc.c  */
#line 7588 "parser.y"
    {
    check_unreached = 0;
  ;}
    break;

  case 1410:

/* Line 1455 of yacc.c  */
#line 7592 "parser.y"
    {
    current_statement->handler2 = (yyvsp[(3) - (3)]);
  ;}
    break;

  case 1411:

/* Line 1455 of yacc.c  */
#line 7604 "parser.y"
    {
        current_statement->handler_id = COB_EC_OVERFLOW;
  ;}
    break;

  case 1413:

/* Line 1455 of yacc.c  */
#line 7611 "parser.y"
    {
    check_unreached = 0;
  ;}
    break;

  case 1414:

/* Line 1455 of yacc.c  */
#line 7615 "parser.y"
    {
    current_statement->handler1 = (yyvsp[(3) - (3)]);
  ;}
    break;

  case 1416:

/* Line 1455 of yacc.c  */
#line 7622 "parser.y"
    {
    check_unreached = 0;
  ;}
    break;

  case 1417:

/* Line 1455 of yacc.c  */
#line 7626 "parser.y"
    {
    current_statement->handler2 = (yyvsp[(3) - (3)]);
  ;}
    break;

  case 1418:

/* Line 1455 of yacc.c  */
#line 7638 "parser.y"
    {
        current_statement->handler_id = COB_EC_I_O_AT_END;
        current_statement->handler1 = (yyvsp[(1) - (1)]);
  ;}
    break;

  case 1419:

/* Line 1455 of yacc.c  */
#line 7643 "parser.y"
    {
        current_statement->handler_id = COB_EC_I_O_AT_END;
        current_statement->handler2 = (yyvsp[(1) - (1)]);
  ;}
    break;

  case 1420:

/* Line 1455 of yacc.c  */
#line 7648 "parser.y"
    {
        current_statement->handler_id = COB_EC_I_O_AT_END;
        current_statement->handler1 = (yyvsp[(1) - (2)]);
        current_statement->handler2 = (yyvsp[(2) - (2)]);
  ;}
    break;

  case 1421:

/* Line 1455 of yacc.c  */
#line 7657 "parser.y"
    {
    check_unreached = 0;
  ;}
    break;

  case 1422:

/* Line 1455 of yacc.c  */
#line 7661 "parser.y"
    {
    (yyval) = (yyvsp[(3) - (3)]);
  ;}
    break;

  case 1423:

/* Line 1455 of yacc.c  */
#line 7668 "parser.y"
    {
    check_unreached = 0;
  ;}
    break;

  case 1424:

/* Line 1455 of yacc.c  */
#line 7672 "parser.y"
    {
    (yyval) = (yyvsp[(3) - (3)]);
  ;}
    break;

  case 1425:

/* Line 1455 of yacc.c  */
#line 7684 "parser.y"
    {
        current_statement->handler_id = COB_EC_I_O_EOP;
        current_statement->handler1 = (yyvsp[(1) - (1)]);
  ;}
    break;

  case 1426:

/* Line 1455 of yacc.c  */
#line 7689 "parser.y"
    {
        current_statement->handler_id = COB_EC_I_O_EOP;
        current_statement->handler2 = (yyvsp[(1) - (1)]);
  ;}
    break;

  case 1427:

/* Line 1455 of yacc.c  */
#line 7694 "parser.y"
    {
        current_statement->handler_id = COB_EC_I_O_EOP;
        current_statement->handler1 = (yyvsp[(1) - (2)]);
        current_statement->handler2 = (yyvsp[(2) - (2)]);
  ;}
    break;

  case 1428:

/* Line 1455 of yacc.c  */
#line 7703 "parser.y"
    {
    check_unreached = 0;
  ;}
    break;

  case 1429:

/* Line 1455 of yacc.c  */
#line 7707 "parser.y"
    {
    (yyval) = (yyvsp[(3) - (3)]);
  ;}
    break;

  case 1430:

/* Line 1455 of yacc.c  */
#line 7714 "parser.y"
    {
    check_unreached = 0;
  ;}
    break;

  case 1431:

/* Line 1455 of yacc.c  */
#line 7718 "parser.y"
    {
    (yyval) = (yyvsp[(3) - (3)]);
  ;}
    break;

  case 1434:

/* Line 1455 of yacc.c  */
#line 7734 "parser.y"
    {
        current_statement->handler_id = COB_EC_I_O_INVALID_KEY;
        current_statement->handler1 = (yyvsp[(1) - (1)]);
  ;}
    break;

  case 1435:

/* Line 1455 of yacc.c  */
#line 7739 "parser.y"
    {
        current_statement->handler_id = COB_EC_I_O_INVALID_KEY;
        current_statement->handler2 = (yyvsp[(1) - (1)]);
  ;}
    break;

  case 1436:

/* Line 1455 of yacc.c  */
#line 7744 "parser.y"
    {
        current_statement->handler_id = COB_EC_I_O_INVALID_KEY;
        current_statement->handler1 = (yyvsp[(1) - (2)]);
        current_statement->handler2 = (yyvsp[(2) - (2)]);
  ;}
    break;

  case 1437:

/* Line 1455 of yacc.c  */
#line 7753 "parser.y"
    {
    check_unreached = 0;
  ;}
    break;

  case 1438:

/* Line 1455 of yacc.c  */
#line 7757 "parser.y"
    {
    (yyval) = (yyvsp[(3) - (3)]);
  ;}
    break;

  case 1439:

/* Line 1455 of yacc.c  */
#line 7764 "parser.y"
    {
    check_unreached = 0;
  ;}
    break;

  case 1440:

/* Line 1455 of yacc.c  */
#line 7768 "parser.y"
    {
    (yyval) = (yyvsp[(3) - (3)]);
  ;}
    break;

  case 1441:

/* Line 1455 of yacc.c  */
#line 7778 "parser.y"
    { BEGIN_STATEMENT ("XML", TERM_XML); 
                             ;}
    break;

  case 1443:

/* Line 1455 of yacc.c  */
#line 7787 "parser.y"
    {
      cb_emit_xml_parse((yyvsp[(2) - (8)]), (yyvsp[(8) - (8)]), (yyvsp[(4) - (8)]), (yyvsp[(5) - (8)]));   
   ;}
    break;

  case 1444:

/* Line 1455 of yacc.c  */
#line 7791 "parser.y"
    {
      cb_emit_xml_generate((yyvsp[(2) - (5)]), (yyvsp[(4) - (5)]), (yyvsp[(5) - (5)]));         
   ;}
    break;

  case 1448:

/* Line 1455 of yacc.c  */
#line 7804 "parser.y"
    { (yyval) = NULL;;}
    break;

  case 1449:

/* Line 1455 of yacc.c  */
#line 7805 "parser.y"
    { (yyval) = (yyvsp[(3) - (3)]); ;}
    break;

  case 1450:

/* Line 1455 of yacc.c  */
#line 7809 "parser.y"
    { terminator_warning (TERM_XML); ;}
    break;

  case 1451:

/* Line 1455 of yacc.c  */
#line 7810 "parser.y"
    { terminator_clear (TERM_XML); ;}
    break;

  case 1452:

/* Line 1455 of yacc.c  */
#line 7814 "parser.y"
    { (yyval) = NULL;;}
    break;

  case 1453:

/* Line 1455 of yacc.c  */
#line 7815 "parser.y"
    { (yyval) = cb_int(1); ;}
    break;

  case 1454:

/* Line 1455 of yacc.c  */
#line 7819 "parser.y"
    { (yyval) = NULL;;}
    break;

  case 1455:

/* Line 1455 of yacc.c  */
#line 7820 "parser.y"
    { (yyval) = (yyvsp[(2) - (2)]); ;}
    break;

  case 1456:

/* Line 1455 of yacc.c  */
#line 7829 "parser.y"
    {
    (yyval) = cb_one;
  ;}
    break;

  case 1457:

/* Line 1455 of yacc.c  */
#line 7833 "parser.y"
    {
    (yyval) = (yyvsp[(2) - (3)]);
  ;}
    break;

  case 1458:

/* Line 1455 of yacc.c  */
#line 7845 "parser.y"
    {
        (yyval) = cb_build_cond ((yyvsp[(1) - (1)]));
  ;}
    break;

  case 1459:

/* Line 1455 of yacc.c  */
#line 7852 "parser.y"
    {
        (yyval) = cb_build_expr ((yyvsp[(1) - (1)]));
  ;}
    break;

  case 1460:

/* Line 1455 of yacc.c  */
#line 7858 "parser.y"
    {
        current_expr = NULL;
  ;}
    break;

  case 1461:

/* Line 1455 of yacc.c  */
#line 7862 "parser.y"
    {
        (yyval) = cb_list_reverse (current_expr);
  ;}
    break;

  case 1464:

/* Line 1455 of yacc.c  */
#line 7874 "parser.y"
    { push_expr ('x', (yyvsp[(1) - (1)])); ;}
    break;

  case 1465:

/* Line 1455 of yacc.c  */
#line 7876 "parser.y"
    { push_expr ('(', NULL); ;}
    break;

  case 1466:

/* Line 1455 of yacc.c  */
#line 7877 "parser.y"
    { push_expr (')', NULL); ;}
    break;

  case 1467:

/* Line 1455 of yacc.c  */
#line 7879 "parser.y"
    { push_expr ('+', NULL); ;}
    break;

  case 1468:

/* Line 1455 of yacc.c  */
#line 7880 "parser.y"
    { push_expr ('-', NULL); ;}
    break;

  case 1469:

/* Line 1455 of yacc.c  */
#line 7881 "parser.y"
    { push_expr ('*', NULL); ;}
    break;

  case 1470:

/* Line 1455 of yacc.c  */
#line 7882 "parser.y"
    { push_expr ('/', NULL); ;}
    break;

  case 1471:

/* Line 1455 of yacc.c  */
#line 7883 "parser.y"
    { push_expr ('^', NULL); ;}
    break;

  case 1472:

/* Line 1455 of yacc.c  */
#line 7884 "parser.y"
    { push_expr ('a', NULL); ;}
    break;

  case 1473:

/* Line 1455 of yacc.c  */
#line 7885 "parser.y"
    { push_expr ('o', NULL); ;}
    break;

  case 1474:

/* Line 1455 of yacc.c  */
#line 7886 "parser.y"
    { push_expr ('y', NULL); ;}
    break;

  case 1475:

/* Line 1455 of yacc.c  */
#line 7887 "parser.y"
    { push_expr ('n', NULL); ;}
    break;

  case 1476:

/* Line 1455 of yacc.c  */
#line 7889 "parser.y"
    { push_expr ('=', NULL); ;}
    break;

  case 1477:

/* Line 1455 of yacc.c  */
#line 7890 "parser.y"
    { push_expr ('>', NULL); ;}
    break;

  case 1478:

/* Line 1455 of yacc.c  */
#line 7891 "parser.y"
    { push_expr ('<', NULL); ;}
    break;

  case 1479:

/* Line 1455 of yacc.c  */
#line 7892 "parser.y"
    { push_expr (']', NULL); ;}
    break;

  case 1480:

/* Line 1455 of yacc.c  */
#line 7893 "parser.y"
    { push_expr ('[', NULL); ;}
    break;

  case 1481:

/* Line 1455 of yacc.c  */
#line 7894 "parser.y"
    { push_expr ('~', NULL); ;}
    break;

  case 1482:

/* Line 1455 of yacc.c  */
#line 7895 "parser.y"
    { push_expr ('~', NULL); ;}
    break;

  case 1483:

/* Line 1455 of yacc.c  */
#line 7896 "parser.y"
    { push_expr ('[', NULL); ;}
    break;

  case 1484:

/* Line 1455 of yacc.c  */
#line 7897 "parser.y"
    { push_expr (']', NULL); ;}
    break;

  case 1485:

/* Line 1455 of yacc.c  */
#line 7898 "parser.y"
    { push_expr ('<', NULL); ;}
    break;

  case 1486:

/* Line 1455 of yacc.c  */
#line 7899 "parser.y"
    { push_expr ('>', NULL); ;}
    break;

  case 1487:

/* Line 1455 of yacc.c  */
#line 7900 "parser.y"
    { push_expr ('=', NULL); ;}
    break;

  case 1488:

/* Line 1455 of yacc.c  */
#line 7902 "parser.y"
    { push_expr ('!', NULL); ;}
    break;

  case 1489:

/* Line 1455 of yacc.c  */
#line 7903 "parser.y"
    { push_expr ('&', NULL); ;}
    break;

  case 1490:

/* Line 1455 of yacc.c  */
#line 7904 "parser.y"
    { push_expr ('|', NULL); ;}
    break;

  case 1491:

/* Line 1455 of yacc.c  */
#line 7906 "parser.y"
    { push_expr ('O', NULL); ;}
    break;

  case 1492:

/* Line 1455 of yacc.c  */
#line 7907 "parser.y"
    { push_expr ('9', NULL); ;}
    break;

  case 1493:

/* Line 1455 of yacc.c  */
#line 7908 "parser.y"
    { push_expr ('A', NULL); ;}
    break;

  case 1494:

/* Line 1455 of yacc.c  */
#line 7909 "parser.y"
    { push_expr ('L', NULL); ;}
    break;

  case 1495:

/* Line 1455 of yacc.c  */
#line 7910 "parser.y"
    { push_expr ('U', NULL); ;}
    break;

  case 1496:

/* Line 1455 of yacc.c  */
#line 7913 "parser.y"
    { push_expr ('P', NULL); ;}
    break;

  case 1497:

/* Line 1455 of yacc.c  */
#line 7914 "parser.y"
    { push_expr ('N', NULL); ;}
    break;

  case 1498:

/* Line 1455 of yacc.c  */
#line 7915 "parser.y"
    { push_expr ('S', NULL); ;}
    break;

  case 1499:

/* Line 1455 of yacc.c  */
#line 7916 "parser.y"
    { push_expr ('F', NULL); ;}
    break;

  case 1512:

/* Line 1455 of yacc.c  */
#line 7929 "parser.y"
    { (yyval) = cb_list_init ((yyvsp[(1) - (1)])); ;}
    break;

  case 1513:

/* Line 1455 of yacc.c  */
#line 7930 "parser.y"
    { (yyval) = cb_list_add ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])); ;}
    break;

  case 1514:

/* Line 1455 of yacc.c  */
#line 7934 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);;}
    break;

  case 1515:

/* Line 1455 of yacc.c  */
#line 7935 "parser.y"
    {(yyval) = cb_int0; ;}
    break;

  case 1516:

/* Line 1455 of yacc.c  */
#line 7939 "parser.y"
    { (yyval) = cb_list_init ((yyvsp[(1) - (1)])); ;}
    break;

  case 1517:

/* Line 1455 of yacc.c  */
#line 7940 "parser.y"
    { (yyval) = cb_list_add ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])); ;}
    break;

  case 1521:

/* Line 1455 of yacc.c  */
#line 7949 "parser.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 1522:

/* Line 1455 of yacc.c  */
#line 7950 "parser.y"
    { (yyval) = cb_build_binary_op ((yyvsp[(1) - (3)]), '+', (yyvsp[(3) - (3)])); ;}
    break;

  case 1523:

/* Line 1455 of yacc.c  */
#line 7951 "parser.y"
    { (yyval) = cb_build_binary_op ((yyvsp[(1) - (3)]), '-', (yyvsp[(3) - (3)])); ;}
    break;

  case 1524:

/* Line 1455 of yacc.c  */
#line 7952 "parser.y"
    { (yyval) = cb_build_binary_op ((yyvsp[(1) - (3)]), '*', (yyvsp[(3) - (3)])); ;}
    break;

  case 1525:

/* Line 1455 of yacc.c  */
#line 7953 "parser.y"
    { (yyval) = cb_build_binary_op ((yyvsp[(1) - (3)]), '/', (yyvsp[(3) - (3)])); ;}
    break;

  case 1526:

/* Line 1455 of yacc.c  */
#line 7954 "parser.y"
    { (yyval) = (yyvsp[(2) - (2)]); ;}
    break;

  case 1527:

/* Line 1455 of yacc.c  */
#line 7955 "parser.y"
    { (yyval) = cb_build_binary_op (cb_zero, '-', (yyvsp[(2) - (2)])); /*  cb_build_sign_value($2);*/;}
    break;

  case 1528:

/* Line 1455 of yacc.c  */
#line 7956 "parser.y"
    { (yyval) = cb_build_binary_op ((yyvsp[(1) - (3)]), '^', (yyvsp[(3) - (3)])); ;}
    break;

  case 1529:

/* Line 1455 of yacc.c  */
#line 7957 "parser.y"
    { (yyval) = (yyvsp[(2) - (3)]); ;}
    break;

  case 1530:

/* Line 1455 of yacc.c  */
#line 7958 "parser.y"
    { (yyval) = cb_build_binary_op ((yyvsp[(1) - (3)]), 'a', (yyvsp[(3) - (3)])); ;}
    break;

  case 1531:

/* Line 1455 of yacc.c  */
#line 7959 "parser.y"
    { (yyval) = cb_build_binary_op ((yyvsp[(1) - (3)]), 'o', (yyvsp[(3) - (3)])); ;}
    break;

  case 1532:

/* Line 1455 of yacc.c  */
#line 7960 "parser.y"
    { (yyval) = cb_build_binary_op ((yyvsp[(1) - (3)]), 'y', (yyvsp[(3) - (3)])); ;}
    break;

  case 1533:

/* Line 1455 of yacc.c  */
#line 7961 "parser.y"
    { (yyval) = cb_build_not_value ((yyvsp[(2) - (2)])); ;}
    break;

  case 1534:

/* Line 1455 of yacc.c  */
#line 7973 "parser.y"
    {
        if (current_linage > 1) {
                cb_error (_("LINAGE-COUNTER must be qualified here"));
                (yyval) = cb_error_node;
        } else if (current_linage == 0) {
                cb_error (_("Invalid LINAGE-COUNTER usage"));
                (yyval) = cb_error_node;
        } else {
                (yyval) = linage_file->linage_ctr;
        }
  ;}
    break;

  case 1535:

/* Line 1455 of yacc.c  */
#line 7985 "parser.y"
    {
        if (CB_FILE_P (cb_ref ((yyvsp[(3) - (3)])))) {
                (yyval) = CB_FILE (cb_ref ((yyvsp[(3) - (3)])))->linage_ctr;
        } else {
                cb_error_x ((yyvsp[(3) - (3)]), _("'%s' is not a file name"), CB_NAME ((yyvsp[(3) - (3)])));
                (yyval) = cb_error_node;
        }
  ;}
    break;

  case 1536:

/* Line 1455 of yacc.c  */
#line 7999 "parser.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 1537:

/* Line 1455 of yacc.c  */
#line 8001 "parser.y"
    { (yyval) = cb_list_append ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])); ;}
    break;

  case 1538:

/* Line 1455 of yacc.c  */
#line 8005 "parser.y"
    { (yyval) = cb_build_pair ((yyvsp[(2) - (2)]), (yyvsp[(1) - (2)])); ;}
    break;

  case 1539:

/* Line 1455 of yacc.c  */
#line 8011 "parser.y"
    { cb_build_identifier ((yyvsp[(1) - (1)])); ;}
    break;

  case 1540:

/* Line 1455 of yacc.c  */
#line 8018 "parser.y"
    {
        cb_tree x;

        x = cb_ref ((yyvsp[(1) - (1)]));
        if (!CB_FIELD_P (x)) {
                (yyval) = cb_error_node;
        } else if (!CB_FIELD (x)->index_list) {
                cb_error_x ((yyvsp[(1) - (1)]), _("'%s' not indexed"), cb_name ((yyvsp[(1) - (1)])));
                cb_error_x (x, _("'%s' defined here"), cb_name (x));
                (yyval) = cb_error_node;
        } else {
                (yyval) = (yyvsp[(1) - (1)]);
        }
  ;}
    break;

  case 1541:

/* Line 1455 of yacc.c  */
#line 8038 "parser.y"
    {
    (yyval) = cb_list_init ((yyvsp[(1) - (1)]));
  ;}
    break;

  case 1542:

/* Line 1455 of yacc.c  */
#line 8042 "parser.y"
    {
    cb_tree     l;

    if ((yyvsp[(2) - (2)]) != cb_error_node) {
        for (l = (yyvsp[(1) - (2)]); l; l = CB_CHAIN (l)) {
            if (!strcasecmp (CB_NAME ((yyvsp[(2) - (2)])), CB_NAME (CB_VALUE (l)))) {
                cb_error_x ((yyvsp[(2) - (2)]), _("Multiple reference to '%s' "), CB_NAME ((yyvsp[(2) - (2)])));
            }
        }
        (yyval) = cb_list_add ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]));
    }
  ;}
    break;

  case 1543:

/* Line 1455 of yacc.c  */
#line 8058 "parser.y"
    {
        /*
        if (CB_FILE_P (cb_ref ($1))) {
                $$ = $1;
        } else {
        */
            cb_tree x;
            x = cb_build_file_reference(CB_NAME((yyvsp[(1) - (1)])));
            if (CB_FILE_P (cb_ref (x))) {
                (yyval) = x;          
            } else {
                    cb_error_x ((yyvsp[(1) - (1)]), _("'%s' is not a file name"), CB_NAME ((yyvsp[(1) - (1)])));
                    (yyval) = cb_error_node;
            }
        /*}*/
  ;}
    break;

  case 1544:

/* Line 1455 of yacc.c  */
#line 8079 "parser.y"
    { (yyval) = cb_list_init ((yyvsp[(1) - (1)])); ;}
    break;

  case 1545:

/* Line 1455 of yacc.c  */
#line 8081 "parser.y"
    { (yyval) = cb_list_add ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])); ;}
    break;

  case 1546:

/* Line 1455 of yacc.c  */
#line 8085 "parser.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 1547:

/* Line 1455 of yacc.c  */
#line 8091 "parser.y"
    { (yyval) = NULL; ;}
    break;

  case 1548:

/* Line 1455 of yacc.c  */
#line 8093 "parser.y"
    { (yyval) = cb_list_add ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])); ;}
    break;

  case 1549:

/* Line 1455 of yacc.c  */
#line 8098 "parser.y"
    {
        (yyval) = (yyvsp[(1) - (1)]);
        CB_REFERENCE ((yyval))->offset = CB_TREE (current_section);
        current_program->label_list = cb_cons ((yyval), current_program->label_list);
  ;}
    break;

  case 1553:

/* Line 1455 of yacc.c  */
#line 8113 "parser.y"
    {
        if (CB_LITERAL((yyvsp[(1) - (1)])) -> image) {
           (yyval) = cb_build_label_reference(cb_build_reference ((char *)(CB_LITERAL ((yyvsp[(1) - (1)]))->image)));
        } else {
           (yyval) = cb_build_label_reference(cb_build_reference ((char *)(CB_LITERAL ((yyvsp[(1) - (1)]))->data)));
        }
        (yyval)->source_file = (yyvsp[(1) - (1)])->source_file;
        (yyval)->source_line = (yyvsp[(1) - (1)])->source_line;
  ;}
    break;

  case 1554:

/* Line 1455 of yacc.c  */
#line 8127 "parser.y"
    { (yyval) = cb_list_init ((yyvsp[(1) - (1)])); ;}
    break;

  case 1555:

/* Line 1455 of yacc.c  */
#line 8128 "parser.y"
    { (yyval) = cb_list_add ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])); ;}
    break;

  case 1556:

/* Line 1455 of yacc.c  */
#line 8133 "parser.y"
    {
        (yyval) = (yyvsp[(1) - (1)]);
        if (CB_REFERENCE_P((yyval)))
            CB_REFERENCE((yyval))->cb_ref_optinal = cb_warn_undefine;
        current_program->reference_list = cb_cons ((yyval), current_program->reference_list);
  ;}
    break;

  case 1557:

/* Line 1455 of yacc.c  */
#line 8142 "parser.y"
    { (yyval) = NULL; ;}
    break;

  case 1558:

/* Line 1455 of yacc.c  */
#line 8143 "parser.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 1561:

/* Line 1455 of yacc.c  */
#line 8155 "parser.y"
    {
        (yyval) = (yyvsp[(1) - (1)]);
        if (CB_REFERENCE ((yyval))->word->count > 0) {                
                redefinition_error ((yyval));
                (yyval) = cb_error_node;
        }
  ;}
    break;

  case 1562:

/* Line 1455 of yacc.c  */
#line 8165 "parser.y"
    {(yyval) = NULL;;}
    break;

  case 1563:

/* Line 1455 of yacc.c  */
#line 8166 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);;}
    break;

  case 1564:

/* Line 1455 of yacc.c  */
#line 8178 "parser.y"
    { (yyval) = cb_list_init ((yyvsp[(1) - (1)])); ;}
    break;

  case 1565:

/* Line 1455 of yacc.c  */
#line 8179 "parser.y"
    { (yyval) = cb_list_add ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])); ;}
    break;

  case 1566:

/* Line 1455 of yacc.c  */
#line 8180 "parser.y"
    { (yyval) = cb_list_add ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])); ;}
    break;

  case 1568:

/* Line 1455 of yacc.c  */
#line 8185 "parser.y"
    { (yyval) = cb_build_address ((yyvsp[(3) - (3)])); ;}
    break;

  case 1569:

/* Line 1455 of yacc.c  */
#line 8189 "parser.y"
    { (yyval) = cb_list_init ((yyvsp[(1) - (1)])); ;}
    break;

  case 1570:

/* Line 1455 of yacc.c  */
#line 8190 "parser.y"
    { (yyval) = cb_list_add ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])); ;}
    break;

  case 1572:

/* Line 1455 of yacc.c  */
#line 8195 "parser.y"
    { (yyval) = cb_build_length ((yyvsp[(3) - (3)])); ;}
    break;

  case 1573:

/* Line 1455 of yacc.c  */
#line 8196 "parser.y"
    { (yyval) = cb_build_length ((yyvsp[(3) - (3)])); ;}
    break;

  case 1574:

/* Line 1455 of yacc.c  */
#line 8197 "parser.y"
    { (yyval) = cb_build_length ((yyvsp[(3) - (3)])); ;}
    break;

  case 1575:

/* Line 1455 of yacc.c  */
#line 8198 "parser.y"
    { (yyval) = cb_build_ppointer ((yyvsp[(4) - (4)])); ;}
    break;

  case 1576:

/* Line 1455 of yacc.c  */
#line 8199 "parser.y"
    { (yyval) = cb_build_address ((yyvsp[(3) - (3)])); ;}
    break;

  case 1581:

/* Line 1455 of yacc.c  */
#line 8207 "parser.y"
    { (yyval) = cb_build_length ((yyvsp[(3) - (3)])); ;}
    break;

  case 1582:

/* Line 1455 of yacc.c  */
#line 8208 "parser.y"
    { (yyval) = cb_build_length ((yyvsp[(3) - (3)])); ;}
    break;

  case 1583:

/* Line 1455 of yacc.c  */
#line 8209 "parser.y"
    { (yyval) = cb_build_length ((yyvsp[(3) - (3)])); ;}
    break;

  case 1589:

/* Line 1455 of yacc.c  */
#line 8221 "parser.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 1590:

/* Line 1455 of yacc.c  */
#line 8222 "parser.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 1597:

/* Line 1455 of yacc.c  */
#line 8245 "parser.y"
    { (yyval) = NULL; ;}
    break;

  case 1602:

/* Line 1455 of yacc.c  */
#line 8257 "parser.y"
    { (yyval) = NULL; ;}
    break;

  case 1606:

/* Line 1455 of yacc.c  */
#line 8264 "parser.y"
    { (yyval) = cb_zero; ;}
    break;

  case 1609:

/* Line 1455 of yacc.c  */
#line 8270 "parser.y"
    { (yyval) = cb_zero; ;}
    break;

  case 1610:

/* Line 1455 of yacc.c  */
#line 8274 "parser.y"
    { (yyval) = NULL; ;}
    break;

  case 1612:

/* Line 1455 of yacc.c  */
#line 8289 "parser.y"
    { (yyval) = cb_build_identifier ((yyvsp[(1) - (1)])); ;}
    break;

  case 1613:

/* Line 1455 of yacc.c  */
#line 8293 "parser.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 1614:

/* Line 1455 of yacc.c  */
#line 8294 "parser.y"
    { (yyval) = (yyvsp[(1) - (2)]); ;}
    break;

  case 1615:

/* Line 1455 of yacc.c  */
#line 8295 "parser.y"
    { (yyval) = (yyvsp[(1) - (2)]); ;}
    break;

  case 1616:

/* Line 1455 of yacc.c  */
#line 8296 "parser.y"
    { (yyval) = (yyvsp[(1) - (3)]); ;}
    break;

  case 1617:

/* Line 1455 of yacc.c  */
#line 8300 "parser.y"
    { (yyval) = cb_build_label_reference((yyvsp[(1) - (1)])); ;}
    break;

  case 1618:

/* Line 1455 of yacc.c  */
#line 8301 "parser.y"
    { (yyval) = cb_build_label_reference((yyvsp[(1) - (3)])); CB_REFERENCE ((yyval))->chain = (yyvsp[(3) - (3)]); ;}
    break;

  case 1619:

/* Line 1455 of yacc.c  */
#line 8305 "parser.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 1620:

/* Line 1455 of yacc.c  */
#line 8306 "parser.y"
    { (yyval) = (yyvsp[(1) - (3)]); CB_REFERENCE ((yyvsp[(1) - (3)]))->chain = (yyvsp[(3) - (3)]); ;}
    break;

  case 1621:

/* Line 1455 of yacc.c  */
#line 8307 "parser.y"
    { (yyval) = (yyvsp[(2) - (2)]); CB_REFERENCE ((yyvsp[(2) - (2)]))->chain = (yyvsp[(1) - (2)]); ;}
    break;

  case 1622:

/* Line 1455 of yacc.c  */
#line 8312 "parser.y"
    {
        (yyval) = (yyvsp[(0) - (3)]);
        if ((yyvsp[(2) - (3)]))
            CB_REFERENCE ((yyvsp[(0) - (3)]))->subs = cb_list_reverse ((yyvsp[(2) - (3)]));
  ;}
    break;

  case 1623:

/* Line 1455 of yacc.c  */
#line 8321 "parser.y"
    {
        CB_REFERENCE ((yyvsp[(0) - (4)]))->offset = (yyvsp[(2) - (4)]);
  ;}
    break;

  case 1624:

/* Line 1455 of yacc.c  */
#line 8325 "parser.y"
    {
        CB_REFERENCE ((yyvsp[(0) - (5)]))->offset = (yyvsp[(2) - (5)]);
        CB_REFERENCE ((yyvsp[(0) - (5)]))->length = (yyvsp[(4) - (5)]);
  ;}
    break;

  case 1625:

/* Line 1455 of yacc.c  */
#line 8337 "parser.y"
    {
    if (cb_tree_category ((yyvsp[(1) - (1)])) != CB_CATEGORY_NUMERIC) {
        cb_error (_("Integer value expected"));
    } else if (CB_LITERAL ((yyvsp[(1) - (1)]))->sign < 0 || CB_LITERAL ((yyvsp[(1) - (1)]))->scale) {
        cb_error (_("Integer value expected"));
    }
    (yyval) = (yyvsp[(1) - (1)]);
  ;}
    break;

  case 1626:

/* Line 1455 of yacc.c  */
#line 8349 "parser.y"
    {
    if (cb_tree_category ((yyvsp[(1) - (1)])) != CB_CATEGORY_NUMERIC) {
        cb_error (_("Integer value expected"));
    } else if (CB_LITERAL ((yyvsp[(1) - (1)]))->scale) {
        cb_error (_("Integer value expected"));
    }
    (yyval) = (yyvsp[(1) - (1)]);
  ;}
    break;

  case 1627:

/* Line 1455 of yacc.c  */
#line 8359 "parser.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 1628:

/* Line 1455 of yacc.c  */
#line 8361 "parser.y"
    {
        (yyval) = (yyvsp[(2) - (2)]);
        if (CB_LITERAL_P ((yyvsp[(2) - (2)]))) {
                CB_LITERAL ((yyvsp[(2) - (2)]))->all = 1;
        }
  ;}
    break;

  case 1629:

/* Line 1455 of yacc.c  */
#line 8370 "parser.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 1630:

/* Line 1455 of yacc.c  */
#line 8371 "parser.y"
    { (yyval) = cb_concat_literals ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])); ;}
    break;

  case 1631:

/* Line 1455 of yacc.c  */
#line 8376 "parser.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 1632:

/* Line 1455 of yacc.c  */
#line 8377 "parser.y"
    { (yyval) = cb_space; ;}
    break;

  case 1633:

/* Line 1455 of yacc.c  */
#line 8378 "parser.y"
    { (yyval) = cb_zero; ;}
    break;

  case 1634:

/* Line 1455 of yacc.c  */
#line 8379 "parser.y"
    { (yyval) = cb_quote; ;}
    break;

  case 1635:

/* Line 1455 of yacc.c  */
#line 8380 "parser.y"
    { (yyval) = cb_dirsep; ;}
    break;

  case 1636:

/* Line 1455 of yacc.c  */
#line 8381 "parser.y"
    { (yyval) = cb_high; ;}
    break;

  case 1637:

/* Line 1455 of yacc.c  */
#line 8382 "parser.y"
    { (yyval) = cb_low; ;}
    break;

  case 1638:

/* Line 1455 of yacc.c  */
#line 8383 "parser.y"
    { (yyval) = cb_null; ;}
    break;

  case 1639:

/* Line 1455 of yacc.c  */
#line 8392 "parser.y"
    {
        (yyval) = cb_build_intrinsic ((yyvsp[(1) - (2)]), NULL, (yyvsp[(2) - (2)]));
  ;}
    break;

  case 1640:

/* Line 1455 of yacc.c  */
#line 8396 "parser.y"
    {
        (yyval) = cb_build_intrinsic ((yyvsp[(1) - (2)]), NULL, (yyvsp[(2) - (2)]));
  ;}
    break;

  case 1641:

/* Line 1455 of yacc.c  */
#line 8400 "parser.y"
    {
    (yyval) = cb_build_intrinsic ((yyvsp[(1) - (5)]), cb_list_init ((yyvsp[(3) - (5)])), (yyvsp[(5) - (5)]));
  ;}
    break;

  case 1642:

/* Line 1455 of yacc.c  */
#line 8404 "parser.y"
    {
    (yyval) = cb_build_intrinsic ((yyvsp[(1) - (5)]), cb_list_init ((yyvsp[(3) - (5)])), (yyvsp[(5) - (5)]));
  ;}
    break;

  case 1643:

/* Line 1455 of yacc.c  */
#line 8408 "parser.y"
    {
    (yyval) = cb_build_intrinsic ((yyvsp[(1) - (5)]), cb_list_init ((yyvsp[(3) - (5)])), (yyvsp[(5) - (5)]));
  ;}
    break;

  case 1644:

/* Line 1455 of yacc.c  */
#line 8412 "parser.y"
    {
    (yyval) = cb_build_intrinsic ((yyvsp[(1) - (5)]), (yyvsp[(3) - (5)]), (yyvsp[(5) - (5)]));
  ;}
    break;

  case 1645:

/* Line 1455 of yacc.c  */
#line 8416 "parser.y"
    {
    (yyval) = cb_build_intrinsic ((yyvsp[(1) - (5)]), (yyvsp[(3) - (5)]), (yyvsp[(5) - (5)]));
  ;}
    break;

  case 1646:

/* Line 1455 of yacc.c  */
#line 8420 "parser.y"
    {
    (yyval) = cb_build_intrinsic ((yyvsp[(1) - (5)]), (yyvsp[(3) - (5)]), (yyvsp[(5) - (5)]));
  ;}
    break;

  case 1647:

/* Line 1455 of yacc.c  */
#line 8424 "parser.y"
    {
        (yyval) = cb_build_intrinsic ((yyvsp[(1) - (5)]), (yyvsp[(3) - (5)]), (yyvsp[(5) - (5)]));
  ;}
    break;

  case 1648:

/* Line 1455 of yacc.c  */
#line 8428 "parser.y"
    {
    (yyval) = cb_build_intrinsic ((yyvsp[(1) - (5)]), cb_list_init ((yyvsp[(3) - (5)])), (yyvsp[(5) - (5)]));
  ;}
    break;

  case 1649:

/* Line 1455 of yacc.c  */
#line 8432 "parser.y"
    {
    (yyval) = cb_build_intrinsic ((yyvsp[(1) - (5)]), cb_list_init ((yyvsp[(3) - (5)])), (yyvsp[(5) - (5)]));
  ;}
    break;

  case 1650:

/* Line 1455 of yacc.c  */
#line 8436 "parser.y"
    {
        (yyval) = cb_build_intrinsic ((yyvsp[(1) - (4)]), (yyvsp[(3) - (4)]), NULL);
  ;}
    break;

  case 1651:

/* Line 1455 of yacc.c  */
#line 8440 "parser.y"
    {
        (yyval) = cb_build_intrinsic ((yyvsp[(1) - (5)]), (yyvsp[(3) - (5)]), (yyvsp[(5) - (5)]));
  ;}
    break;

  case 1652:

/* Line 1455 of yacc.c  */
#line 8444 "parser.y"
    {
    (yyval) = cb_build_intrinsic ((yyvsp[(1) - (5)]), (yyvsp[(3) - (5)]), (yyvsp[(5) - (5)]));
  ;}
    break;

  case 1653:

/* Line 1455 of yacc.c  */
#line 8448 "parser.y"
    {
    (yyval) = cb_build_intrinsic ((yyvsp[(1) - (5)]), (yyvsp[(3) - (5)]), (yyvsp[(5) - (5)]));
  ;}
    break;

  case 1654:

/* Line 1455 of yacc.c  */
#line 8452 "parser.y"
    {
        (yyval) = cb_build_intrinsic ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]), NULL);
  ;}
    break;

  case 1655:

/* Line 1455 of yacc.c  */
#line 8458 "parser.y"
    { (yyval) = NULL; ;}
    break;

  case 1656:

/* Line 1455 of yacc.c  */
#line 8459 "parser.y"
    { (yyval) = cb_build_pair ((yyvsp[(2) - (4)]), NULL); ;}
    break;

  case 1657:

/* Line 1455 of yacc.c  */
#line 8460 "parser.y"
    { (yyval) = cb_build_pair ((yyvsp[(2) - (5)]), (yyvsp[(4) - (5)])); ;}
    break;

  case 1658:

/* Line 1455 of yacc.c  */
#line 8464 "parser.y"
    { (yyval) = NULL; ;}
    break;

  case 1659:

/* Line 1455 of yacc.c  */
#line 8465 "parser.y"
    { (yyval) = (yyvsp[(2) - (3)]);   ;}
    break;

  case 1660:

/* Line 1455 of yacc.c  */
#line 8469 "parser.y"
    { (yyval) = NULL; ;}
    break;

  case 1661:

/* Line 1455 of yacc.c  */
#line 8470 "parser.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 1662:

/* Line 1455 of yacc.c  */
#line 8476 "parser.y"
    {
        cb_tree x;

        x = cb_list_init ((yyvsp[(1) - (1)]));
        (yyval) = cb_list_add (x, cb_int0);
  ;}
    break;

  case 1663:

/* Line 1455 of yacc.c  */
#line 8483 "parser.y"
    {
        cb_tree x;

        x = cb_list_init ((yyvsp[(1) - (3)]));
        (yyval) = cb_list_add (x, cb_int1);
  ;}
    break;

  case 1664:

/* Line 1455 of yacc.c  */
#line 8490 "parser.y"
    {
        cb_tree x;

        x = cb_list_init ((yyvsp[(1) - (3)]));
        (yyval) = cb_list_add (x, cb_int2);
  ;}
    break;

  case 1665:

/* Line 1455 of yacc.c  */
#line 8500 "parser.y"
    {
        cb_tree x;

    x = cb_list_init ((yyvsp[(1) - (1)]));
        (yyval) = cb_list_add (x, cb_null);
  ;}
    break;

  case 1666:

/* Line 1455 of yacc.c  */
#line 8507 "parser.y"
    {
        cb_tree x;

    x = cb_list_init ((yyvsp[(1) - (3)]));
        (yyval) = cb_list_add (x, (yyvsp[(3) - (3)]));
  ;}
    break;

  case 1667:

/* Line 1455 of yacc.c  */
#line 8517 "parser.y"
    {
        cb_tree x;

    x = cb_list_init ((yyvsp[(1) - (1)]));
        (yyval) = cb_list_add (x, cb_null);
  ;}
    break;

  case 1668:

/* Line 1455 of yacc.c  */
#line 8524 "parser.y"
    {
        cb_tree x;

    x = cb_list_init ((yyvsp[(1) - (3)]));
        (yyval) = cb_list_add (x, cb_ref((yyvsp[(3) - (3)])));
  ;}
    break;

  case 1669:

/* Line 1455 of yacc.c  */
#line 8537 "parser.y"
    {
    non_const_word = 1;
  ;}
    break;

  case 1670:

/* Line 1455 of yacc.c  */
#line 8547 "parser.y"
    { (yyval) = cb_int0; ;}
    break;

  case 1671:

/* Line 1455 of yacc.c  */
#line 8548 "parser.y"
    { (yyval) = cb_int1; ;}
    break;

  case 1672:

/* Line 1455 of yacc.c  */
#line 8552 "parser.y"
    { (yyval) = cb_int0; ;}
    break;

  case 1673:

/* Line 1455 of yacc.c  */
#line 8553 "parser.y"
    { (yyval) = cb_int1; ;}
    break;

  case 1674:

/* Line 1455 of yacc.c  */
#line 8557 "parser.y"
    { (yyval) = NULL; ;}
    break;

  case 1675:

/* Line 1455 of yacc.c  */
#line 8558 "parser.y"
    { (yyval) = cb_int1; ;}
    break;

  case 1676:

/* Line 1455 of yacc.c  */
#line 8562 "parser.y"
    { (yyval) = cb_int0; ;}
    break;

  case 1677:

/* Line 1455 of yacc.c  */
#line 8563 "parser.y"
    { (yyval) = cb_int1; ;}
    break;

  case 1678:

/* Line 1455 of yacc.c  */
#line 8564 "parser.y"
    { (yyval) = cb_int2; ;}
    break;

  case 1679:

/* Line 1455 of yacc.c  */
#line 8568 "parser.y"
    { (yyval) = cb_int0; ;}
    break;

  case 1680:

/* Line 1455 of yacc.c  */
#line 8569 "parser.y"
    { (yyval) = cb_int1; ;}
    break;

  case 1681:

/* Line 1455 of yacc.c  */
#line 8573 "parser.y"
    { if (cb_flag_optional_file) (yyval) = cb_int(COB_FILE_OPTIONAL); else (yyval) = cb_int0; ;}
    break;

  case 1682:

/* Line 1455 of yacc.c  */
#line 8574 "parser.y"
    { (yyval) = cb_int(COB_FILE_OPTIONAL_IN_SOURCE); ;}
    break;

  case 1683:

/* Line 1455 of yacc.c  */
#line 8575 "parser.y"
    { (yyval) = cb_int(COB_FILE_OPTIONAL_IN_SOURCE | COB_FILE_OPTIONAL); ;}
    break;

  case 1684:

/* Line 1455 of yacc.c  */
#line 8579 "parser.y"
    { (yyval) = cb_int0; ;}
    break;

  case 1685:

/* Line 1455 of yacc.c  */
#line 8580 "parser.y"
    { (yyval) = cb_int1; rounded_used = 1;;}
    break;

  case 1686:

/* Line 1455 of yacc.c  */
#line 8584 "parser.y"
    { (yyval) = cb_int0; ;}
    break;

  case 1687:

/* Line 1455 of yacc.c  */
#line 8585 "parser.y"
    { (yyval) = cb_int1; ;}
    break;

  case 1715:

/* Line 1455 of yacc.c  */
#line 8605 "parser.y"
    { (yyval) = cb_int1; ;}
    break;

  case 1735:

/* Line 1455 of yacc.c  */
#line 8615 "parser.y"
    { (yyval) = cb_int1; ;}
    break;



/* Line 1455 of yacc.c  */
#line 17278 "parser.c"
      default: break;
    }
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
      {
	YYSIZE_T yysize = yysyntax_error (0, yystate, yychar);
	if (yymsg_alloc < yysize && yymsg_alloc < YYSTACK_ALLOC_MAXIMUM)
	  {
	    YYSIZE_T yyalloc = 2 * yysize;
	    if (! (yysize <= yyalloc && yyalloc <= YYSTACK_ALLOC_MAXIMUM))
	      yyalloc = YYSTACK_ALLOC_MAXIMUM;
	    if (yymsg != yymsgbuf)
	      YYSTACK_FREE (yymsg);
	    yymsg = (char *) YYSTACK_ALLOC (yyalloc);
	    if (yymsg)
	      yymsg_alloc = yyalloc;
	    else
	      {
		yymsg = yymsgbuf;
		yymsg_alloc = sizeof yymsgbuf;
	      }
	  }

	if (0 < yysize && yysize <= yymsg_alloc)
	  {
	    (void) yysyntax_error (yymsg, yystate, yychar);
	    yyerror (yymsg);
	  }
	else
	  {
	    yyerror (YY_("syntax error"));
	    if (yysize != 0)
	      goto yyexhaustedlab;
	  }
      }
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
	{
	  /* Return failure if at end of input.  */
	  if (yychar == YYEOF)
	    YYABORT;
	}
      else
	{
	  yydestruct ("Error: discarding",
		      yytoken, &yylval);
	  yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule which action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;


      yydestruct ("Error: popping",
		  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  *++yyvsp = yylval;


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined(yyoverflow) || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
     yydestruct ("Cleanup: discarding lookahead",
		 yytoken, &yylval);
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}



/* Line 1675 of yacc.c  */
#line 8648 "parser.y"


