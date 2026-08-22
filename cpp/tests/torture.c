/* torture.c — a full, compilable C file that exercises phases 1-4.
 *
 * No system headers, no libc. It compiles and runs standalone:
 *
 *     cc torture.c -o torture && ./torture ; echo $?      # 0 == all pass
 *
 * Once your preprocessor runs:
 *
 *     cpp torture.c > mine.i
 *     cc -E -P torture.c > oracle.i
 *     diff <(cc -E -P -x c mine.i) oracle.i               # token-level oracle
 *     cc mine.i -o mine && ./mine ; echo $?               # semantic oracle
 *
 * A nonzero exit code is the number of the first CHECK that failed.
 * Sections are ordered by how hard they are; delete from the bottom up
 * while you are still building.
 */

/* =====================================================================
 * TIER 0 — include machinery
 * ===================================================================== */

#include "torture.h"
#include "torture.h" /* include guard must swallow this one */

#define HDR "torture.h"
#include HDR /* macro-expanded #include */

#ifdef WITH_STDIO
#include <stdio.h> /* off by default: needs a real search path */
#endif

/* =====================================================================
 * TIER 1 — textual phases (splicing, comments) happen BEFORE tokenizing
 * ===================================================================== */

/* a splice cutting a keyword and a numeric literal in half */
in\
t spliced_decl = 4\
2;

/* a block comment whose opening /* is itself formed by a splice */
/\
* this whole thing is a comment *\
/

/* The cursed case from the README. After splicing and comment removal
   this is exactly:  #define CURSED 1020
   It only works if comments are stripped before directives are found,
   because the block comment eats the newline that would have ended the line. */
/\
*
*/                                                                             \
#/*                                                                            \
  */ defi\
ne CUR\
SED 10\
20

/* digraphs: %: is #, so this is a directive.
   Delete this one line if your host toolchain refuses it. */
%:define DIGRAPH 1

/* =====================================================================
 * TIER 2 — hide set / blue paint. These MUST expand exactly once.
 * The variable is declared before the #define so the declaration itself
 * is not rewritten.
 * ===================================================================== */

static int recur_var = 10;
#define recur_var (1 + recur_var) /* -> (1 + recur_var), inner one painted */

static int p_var = 5;
#define p_var q_var /* p_var -> q_var -> p_var, then painted */
#define q_var p_var

/* =====================================================================
 * TIER 3 — conditionals
 * ===================================================================== */

#if TORTURE_VERSION > 1 && defined(TORTURE_H)
#define BRANCH 1
#elif TORTURE_VERSION == 1
#define BRANCH 2
#else
#error "unreachable: TORTURE_VERSION is neither > 1 nor == 1"
#endif

/* #if arithmetic: char constants, integer division, remainder */
#if 'A' == 65 && (1 / 2) == 0 && (3 % 2) == 1
#define ARITH_OK 1
#else
#define ARITH_OK 0
#endif

/* #if arithmetic is done in the widest integer type, and unsigned wins.
   -1 converts to a huge unsigned value, so this comparison is FALSE. */
#if - 1 < 0u
#define UNSIGNED_OK 0
#else
#define UNSIGNED_OK 1
#endif

/* an identifier that was never defined evaluates to 0 in #if */
#if NEVER_DEFINED == 0
#define UNDEF_IS_ZERO 1
#endif

/* a skipped group is never evaluated: nothing in here has to be valid,
   but nesting still has to be tracked so the right #endif matches */
#if 0
    @ $ ` +++ ]]] this is not C and must never be looked at
#if 1
#error "this must never fire"
#endif
#define MUST_NOT_EXIST 1
#endif

#ifdef MUST_NOT_EXIST
#error "a skipped group was executed"
#endif

/* #undef */
#define TEMP 1
#undef TEMP
#ifdef TEMP
#error "#undef did not work"
#endif

/* unknown pragmas pass through untouched (your cc will warn; that is fine) */
#pragma torture_marker

/* =====================================================================
 * helper — no libc, so compare strings by hand
 * ===================================================================== */

static int str_eq(const char *a, const char *b) {
  while (*a && *a == *b) {
    a++;
    b++;
  }
  return *a == *b;
}

int main(void) {
  int fail = 0;

/* a directive inside a compound statement is still a directive */
#define CHECK(n, cond)                                                         \
  do {                                                                         \
    if (!(cond) && fail == 0)                                                  \
      fail = (n);                                                              \
  } while (0)

  /* ---- tier 1 ---- */
  CHECK(1, spliced_decl == 42);
  CHECK(2, CURSED == 1020);
  CHECK(3, DIGRAPH == 1);

  {
    int arr<:3:> = <%1, 2, 3%>; /* int arr[3] = { 1, 2, 3 }; */
    CHECK(4, arr<:1:> == 2);
  }

  /* ---- object- and function-like ---- */
  CHECK(5, TWO == 2);
  CHECK(6, ADD(1, 2) == 3);
  CHECK(7, MAX(ADD(1, 2), 2) == 3); /* commas inside () do not split */

  {
    /* an invocation may span lines between the name and the ( */
    int m = MAX(3, 7);
    CHECK(8, m == 7);

    /* EMPTY expands to nothing */
    int e = 0 EMPTY;
    CHECK(9, e == 0);

    CHECK(10, ID(4) == 4);
  }

  /* ---- # and ## ---- */
  CHECK(11, str_eq(STR(VAL), "9"));        /* arg expanded before # */
  CHECK(12, str_eq(STR_(VAL), "VAL"));     /* arg NOT expanded */
  CHECK(13, str_eq(STR_("hi"), "\"hi\"")); /* quotes get backslashed */
  CHECK(14, str_eq(STR_(a + b), "a + b")); /* ws runs collapse to one */
  CHECK(15, CAT(A, B) == 7);               /* paste, then rescan AB -> 7 */
  CHECK(16, CAT_(spliced, _decl) == 42);

  /* ---- variadic ---- */
  CHECK(17, COUNT(1, 2, 3) == 3);
  CHECK(18, COUNT(1, 2) == 2);
  CHECK(19, COUNT(1) == 1);

  /* ---- hide set ---- */
  CHECK(20, recur_var == 11); /* (1 + recur_var) == 1 + 10 */
  CHECK(21, p_var == 5);      /* p_var -> q_var -> p_var */

  /* ---- conditionals ---- */
  CHECK(22, BRANCH == 1);
  CHECK(23, ARITH_OK == 1);
  CHECK(24, UNSIGNED_OK == 1);
  CHECK(25, UNDEF_IS_ZERO == 1);

  /* ---- __LINE__ is relative, so this is position independent ---- */
  {
    int l1 = __LINE__;
    int l2 = __LINE__;
    CHECK(26, l2 - l1 == 1);
  }

  /* ---- #line rewrites both counters. keep this last. ---- */
#line 100 "renamed.c"
  CHECK(27, __LINE__ == 101);
  CHECK(28, str_eq(__FILE__, "renamed.c"));

  return fail;
}
