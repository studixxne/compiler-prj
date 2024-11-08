#include <stdio.h>
#include <stdlib.h>
#include "variable.h"
#include "ast.h"

// Create and return a pointer to an AST node that represents integer 'n'.
AST* make_num_ast(int n) {
    AST* ast = (AST*)malloc(sizeof(AST));
    ast->kind = Number;
    ast->num = n;
    ast->id = NULL;
    ast->left = NULL;
    ast->right = NULL;
    return ast;
}

// todo 1
// Create and return a pointer to an AST node that represents identifier 's'.
AST* make_id_ast(char *s) {
    AST* ast = (AST*)malloc(sizeof(AST));
    ast->kind = Identifier;
    ast->num = 0;
    ast->id = s;
    ast->left = NULL;
    ast->right = NULL;
    return ast;
}

// Create and return a pointer to an AST node that represents a negate operation
// of 'oprnd'.
AST* make_neg_ast(AST *oprnd) {
    AST* ast = (AST*)malloc(sizeof(AST));
    ast->kind = Neg;
    ast->num = 0;
    ast->id = NULL;
    ast->left = oprnd;
    ast->right = NULL;
    return ast;
}

// todo 2
// Create and return a pointer to an AST node that represents a binary operation
// with 'oprnd_1' and 'oprnd_2'.
AST* make_binop_ast(AST_KIND kind, AST *oprnd_1, AST *oprnd_2) {
    AST* ast = (AST*)malloc(sizeof(AST));
    ast->kind = kind;
    ast->num = 0;
    ast->id = NULL;
    ast->left = oprnd_1;
    ast->right = oprnd_2;
    return ast;
}

// todo 3
// Return the integer value obtained by evaluating the numeric expression
// represented by the input AST.
int eval_ast(VarNode *vars, AST* ast) {
    switch (ast->kind) {
        case Number:
            return ast->num;

        case Identifier:
            return lookup_var(vars, ast->id);

        case Add:
            return eval_ast(vars, ast->left) + eval_ast(vars, ast->right);

        case Sub:
            return eval_ast(vars, ast->left) - eval_ast(vars, ast->right);

        case Mul:
            return eval_ast(vars, ast->left) * eval_ast(vars, ast->right);

        case Div:
            {
                int divider = eval_ast(vars, ast->right);
                if (divider == 0)
                {
                    printf("Divide Zero\n");
                    exit(-1);
                }

                return eval_ast(vars, ast->left) / divider;
            }

        case Neg:
            return -1 * eval_ast(vars, ast->left);

        default:
            printf("Eval Fail! \n");
            exit(-1);
    }
    return 0;
}
