#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

typedef uint32_t Var;

typedef struct Exp *Exp_p;
typedef struct Env *Env_p;
typedef struct Kont *Kont_p;
typedef struct Val *Val_p;
typedef struct Sigma *Sigma_p;

typedef enum { EXP_REF, EXP_LAM, EXP_APP } ExpTag;
struct Exp {
  ExpTag Tag;
  union {
    struct {
      Var v;
    } Ref;
    struct {
      Exp_p body;
    } Lam;
    struct {
      Exp_p rator;
      Exp_p rand;
    } App;
  } Obj;
};

typedef enum { VAL_CLO } ValTag;
struct Val {
  ValTag Tag;
  union {
    struct {
      Exp_p lam;
      Env_p env;
    } Clo;
  } Obj;
};

struct Env {
  uint8_t count;
  Val_p car;
  Env_p cdr;
};

typedef enum { KONT_MT, KONT_AR, KONT_FN } KontTag;
struct Kont {
  KontTag Tag;
  union {
    void* Mt;
    struct {
      Exp_p rand;
      Env_p env;
      Kont_p k;
    } Ar;
    struct {
      Val_p rator;
      Kont_p k;
    } Fn;
  } Obj;
};

typedef enum { SIG_EVAL, SIG_APPLY } SigmaTag;
struct Sigma {
  SigmaTag Tag;
  union {
    struct {
      Exp_p C;
      Env_p E;
      Kont_p K;
    } Eval;
    struct {
      Val_p V;
      Kont_p K;
    } Apply;
  } Obj;
};

// Dump

void display_Exp(Exp_p e) {
  switch(e->Tag) {
  case EXP_REF: {
    printf("Ref(%d)", e->Obj.Ref.v);
    return;
  }
  case EXP_LAM: {
    printf("Lam(");
    display_Exp(e->Obj.Lam.body);
    printf(")");
    return;
  }
  case EXP_APP: {
    printf("App(");
    display_Exp(e->Obj.App.rator);
    printf(",");
    display_Exp(e->Obj.App.rand);
    printf(")");
    return;
  }
  default:
    exit(1);
  }
}

void display_Env(Env_p env);

void display_Val(Val_p v) {
  switch(v->Tag) {
  case VAL_CLO: {
    printf("Clo(");
    display_Exp(v->Obj.Clo.lam);
    printf(",");
    display_Env(v->Obj.Clo.env);
    printf(")");
    return;
  }
  default:
    exit(1);
  }
}

void display_Env(Env_p env) {
  printf("[");
  while (env) {
    display_Val(env->car);
    env = env->cdr;
  }
  printf("]");
}

void display_Kont(Kont_p k) {
  switch (k->Tag) {
  case KONT_MT: {
    printf("MT");
    return;
  }
  case KONT_AR: {
    printf("Ar(");
    display_Exp(k->Obj.Ar.rand);
    printf(",");
    display_Env(k->Obj.Ar.env);
    printf(",");
    display_Kont(k->Obj.Ar.k);
    printf(")");
    return;
  }
  case KONT_FN: {
    printf("Fn(");
    display_Val(k->Obj.Fn.rator);
    printf(",");
    display_Kont(k->Obj.Fn.k);
    printf(")");
    return;
  }
  default:
    exit(1);
  };
}

void display_Sigma(Sigma_p sig) {
  switch (sig->Tag) {
  case SIG_EVAL: {
    printf("eval(");
    display_Exp(sig->Obj.Eval.C);
    printf(",");
    display_Env(sig->Obj.Eval.E);
    printf(",");
    display_Kont(sig->Obj.Eval.K);
    printf(")");
    return;
  }
  case SIG_APPLY: {
    printf("apply(");
    display_Val(sig->Obj.Apply.V);
    printf(",");
    display_Kont(sig->Obj.Apply.K);
    printf(")");
    return;
  }
  default:
    exit(1);
  };
}

// Code

Exp_p new_Exp() {
  printf("alloc(Exp)\n");
  return (Exp_p)(malloc(sizeof(struct Exp)));
}
Kont_p new_Kont() {
  printf("alloc(Kont)\n");
  return (Kont_p)(malloc(sizeof(struct Kont)));
}
Val_p new_Val() {
  printf("alloc(Val)\n");
  return (Val_p)(malloc(sizeof(struct Val)));
}
Sigma_p new_Sigma() {
  printf("alloc(Sigma)\n");
  return (Sigma_p)(malloc(sizeof(struct Sigma)));
}
Env_p new_Env() {
  printf("alloc(Env)\n");
  Env_p e = (Env_p)(malloc(sizeof(struct Env)));
  e->count = 0;
  return e;
}

void retain_Env(Env_p env) {
  if (env) {
    env->count++;
  }
}
void release_Env(Env_p env) {
  if (env) {
    env->count--;
    if (env->count == 0) {
      free(env);
    }
  }
}

Val_p lookup(Var v, Env_p env) {
  while (v) {
    v--;
    env = env->cdr;
  }
  return env->car;
}

uint8_t step (Sigma_p sig, Sigma_p next_sig) {
  switch (sig->Tag) {
  case SIG_EVAL: {
    switch (sig->Obj.Eval.C->Tag) {
    case EXP_REF: {
      Val_p val = lookup(sig->Obj.Eval.C->Obj.Ref.v, sig->Obj.Eval.E);
      next_sig->Tag = SIG_APPLY;
      next_sig->Obj.Apply.V = val;
      next_sig->Obj.Apply.K = sig->Obj.Eval.K;
      // C & E are dead
      release_Env(sig->Obj.Eval.E);
      return 1;
    }
    case EXP_LAM: {
      Val_p val = new_Val();
      val->Tag = VAL_CLO;
      val->Obj.Clo.lam = sig->Obj.Eval.C->Obj.Lam.body;
      val->Obj.Clo.env = sig->Obj.Eval.E;
      retain_Env(sig->Obj.Eval.E);
      next_sig->Tag = SIG_APPLY;
      next_sig->Obj.Apply.V = val;
      next_sig->Obj.Apply.K = sig->Obj.Eval.K;
      // C is dead
      return 1;
    }
    case EXP_APP: {
      Kont_p kont = new_Kont();
      kont->Tag = KONT_AR;
      kont->Obj.Ar.rand = sig->Obj.Eval.C->Obj.App.rand;
      kont->Obj.Ar.env = sig->Obj.Eval.E;
      retain_Env(sig->Obj.Eval.E);
      kont->Obj.Ar.k = sig->Obj.Eval.K;
      next_sig->Tag = SIG_EVAL;
      next_sig->Obj.Eval.C = sig->Obj.Eval.C->Obj.App.rator;
      next_sig->Obj.Eval.E = sig->Obj.Eval.E;
      next_sig->Obj.Eval.K = kont;
      // C is dead
      return 1;
    }
    default:
      exit(1);
    };
  }
  case SIG_APPLY: {
    switch (sig->Obj.Apply.K->Tag) {
    case KONT_MT: {
      return 0;
    }
    case KONT_AR: {
      Kont_p kont = new_Kont();
      kont->Tag = KONT_FN;
      kont->Obj.Fn.rator = sig->Obj.Apply.V;
      kont->Obj.Fn.k = sig->Obj.Apply.K->Obj.Ar.k;
      next_sig->Tag = SIG_EVAL;
      next_sig->Obj.Eval.C = sig->Obj.Apply.K->Obj.Ar.rand;
      next_sig->Obj.Eval.E = sig->Obj.Apply.K->Obj.Ar.env;
      next_sig->Obj.Eval.K = kont;
      // K is dead
      free(sig->Obj.Apply.K);
      return 1;
    }
    case KONT_FN: {
      Env_p env = new_Env();
      env->car = sig->Obj.Apply.V;
      env->cdr = sig->Obj.Apply.K->Obj.Fn.rator->Obj.Clo.env;
      next_sig->Tag = SIG_EVAL;
      next_sig->Obj.Eval.C = sig->Obj.Apply.K->Obj.Fn.rator->Obj.Clo.lam;
      next_sig->Obj.Eval.E = env;
      retain_Env(env);
      next_sig->Obj.Eval.K = sig->Obj.Apply.K->Obj.Fn.k;
      // K is dead
      free(sig->Obj.Apply.K);
      return 1;
    }
    default:
      exit(1);
    }
  }
  default:
    exit(1);
  }
}

void eval ( Exp_p e ) {
  Sigma_p left = new_Sigma();
  Sigma_p right = new_Sigma();

  Kont_p kont = new_Kont();
  kont->Tag = KONT_MT;

  left->Tag = SIG_EVAL;
  left->Obj.Eval.C = e;
  left->Obj.Eval.E = NULL;
  left->Obj.Eval.K = kont;

  while (1) {
    display_Sigma(left);
    printf("\n");
    if (! step(left, right)) {
      break;
    } else {
      Sigma_p tmp = left;
      left = right;
      right = tmp;
      continue;
    }
  }
}

int main (int argc, char **argv) {
  // omega = \x.x x
  Exp_p arg_ref = new_Exp();
  arg_ref->Tag = EXP_REF;
  arg_ref->Obj.Ref.v = 0;
  Exp_p omega_body = new_Exp();
  omega_body->Tag = EXP_APP;
  omega_body->Obj.App.rator = arg_ref;
  omega_body->Obj.App.rand = arg_ref;
  Exp_p omega = new_Exp();
  omega->Tag = EXP_LAM;
  omega->Obj.Lam.body = omega_body;

  // Omega = (omega omega)
  Exp_p Omega = new_Exp();
  Omega->Tag = EXP_APP;
  Omega->Obj.App.rator = omega;
  Omega->Obj.App.rand = omega;

  eval( Omega );

  return 0;
}
