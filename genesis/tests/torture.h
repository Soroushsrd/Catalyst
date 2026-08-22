/* torture.h — companion header for torture.c
 *
 * Included twice on purpose. The guard must make the second include a no-op.
 */

#ifndef TORTURE_H
#define TORTURE_H

#define TORTURE_VERSION 2

/* ---- object-like ------------------------------------------------------ */
#define ONE 1
#define TWO (ONE + ONE)
#define VAL 9
#define EMPTY /* expands to nothing */

/* ---- function-like ---------------------------------------------------- */
#define ADD(a, b) ((a) + (b))
#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define ID(x) x

/* ---- # and ## --------------------------------------------------------- */
#define STR_(x) #x      /* operand of # : arg NOT pre-expanded */
#define STR(x) STR_(x)  /* one level of indirection -> expanded */
#define CAT_(a, b) a##b /* operands of ## : NOT pre-expanded    */
#define CAT(a, b) CAT_(a, b)
#define AB 7 /* CAT(A,B) must rescan into 7          */

/* ---- variadic --------------------------------------------------------- */
#define COUNT(...) COUNT_(__VA_ARGS__, 3, 2, 1, 0)
#define COUNT_(_1, _2, _3, N, ...) N

#endif /* TORTURE_H */
