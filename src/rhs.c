#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <limits.h>
#include <math.h>

/* Storage is allocated by initmod inside deSolve's .Call and lives until that
   solve returns (including errors). Solves must be serial within an R process;
   process-based parallel workers have independent storage. */
static int npar, S, NR;
static double *uptake, *p, temperature, trials, minimum, minimum_time;
SEXP cs_prepare(SEXP length) {
    npar = asInteger(length);
    if (npar < 2) error("Invalid compiled RHS parameter length.");
    return R_NilValue;
}
void cs_initmod(void (*odeparms)(int *, double *)) {
    p = (double *) R_alloc(npar, sizeof(double));
    odeparms(&npar, p);
    if (!R_FINITE(p[0]) || p[0] < 1 || p[0] > INT_MAX || p[0] != floor(p[0]) ||
        !R_FINITE(p[1]) || p[1] < 0 || p[1] > INT_MAX || p[1] != floor(p[1]))
        error("Invalid compiled RHS dimensions.");
    S = (int)p[0]; NR = (int)p[1];
    double expected = NR ? 4.0 + 5.0*S*NR + 2.0*S + 2.0*NR : 5.0 + (double)S*S + 5.0*S;
    if (expected != npar) error("Invalid compiled RHS parameter dimensions.");
    uptake = (double *) R_alloc((size_t)S * NR, sizeof(double));
    trials = 0; minimum = 0; minimum_time = NA_REAL;
}
void cs_initforc(void (*odeforcs)(int *, double *)) {
    int n = 1; odeforcs(&n, &temperature);
}
SEXP cs_trials(void) {
    SEXP ans = PROTECT(allocVector(REALSXP, 3));
    REAL(ans)[0] = trials; REAL(ans)[1] = minimum; REAL(ans)[2] = minimum_time;
    UNPROTECT(1); return ans;
}
static double clamp(double x) { return x < 0 ? 0 : x; }
static void check_state(double *y, int n, double limit, int lv, double t) {
    int negative = 0;
    for (int i = 0; i < n; ++i) {
        if (!R_FINITE(y[i])) error(lv ? "Non-finite abundance encountered by ODE solver." : "Non-finite state encountered by ODE solver.");
    }
    for (int i = 0; i < n; ++i) {
        if (y[i] > limit) error(lv ? "Abundance exceeded blow-up threshold." : "State exceeded blow-up threshold.");
        if (y[i] < 0) negative = 1;
        if (lv && y[i] < minimum) { minimum = y[i]; minimum_time = t; }
    }
    if (lv && negative) trials++;
}
void cs_cr(int *neq, double *t, double *y, double *dy, double *out, int *ip) {
    double *a=p+4, *opt=a+S*NR, *sd=opt+S*NR, *use=sd+S*NR;
    double *h=use+S*NR, *e=h+S*NR, *d=e+S, *rho=d+S, *K=rho+NR;
    if (*neq != S + NR) error("Invalid compiled RHS state dimensions.");
    if (ip[0] > 0) out[0] = temperature;
    check_state(y, *neq, p[3], 0, *t);
    /* Extended accumulators match R's rowSums/colSums accumulation. */
    /* No allocation in the callback. */
    for (int i=0; i<S; ++i) {
        long double sum=0;
        for (int j=0; j<NR; ++j) {
            int k=i+S*j; double r=clamp(y[S+j]);
            double z=(temperature-opt[k])/sd[k];
            double u=a[k]*exp(-0.5*z*z);
            uptake[k] = use[k]*u*r/(h[k]+r);
            sum += uptake[k];
        }
        dy[i]=clamp(y[i])*(e[i]*(double)sum-d[i])+p[2];
    }
    for (int j=0; j<NR; ++j) {
        long double sum=0; double r=clamp(y[S+j]);
        for (int i=0; i<S; ++i) {
            int k=i+S*j;
            sum += clamp(y[i])*uptake[k];
        }
        dy[S+j]=rho[j]*(K[j]-r)-(double)sum;
    }
    for (int i=0; i<*neq; ++i) if (!R_FINITE(dy[i])) error("Non-finite derivative encountered by ODE solver.");
}
void cs_lv(int *neq, double *t, double *y, double *dy, double *out, int *ip) {
    double *al=p+5, *opt=al+S*S, *sd=opt+S, *ab=sd+S, *ad=ab+S, *z=ad+S;
    if (*neq != S) error("Invalid compiled RHS state dimensions.");
    if (ip[0] > 0) out[0] = temperature;
    check_state(y, *neq, p[3], 1, *t);
    for (int i=0; i<S; ++i) {
        double sum=0, v=(temperature-opt[i])/sd[i];
        for (int j=0; j<S; ++j) sum += al[i+S*j]*clamp(y[j]);
        double rms=ab[i]*exp(-0.5*v*v)-ad[i]*exp(z[i]*temperature)+p[4];
        double K=rms/0.002;
        dy[i]=clamp(y[i])*(rms*(1-sum/K))+p[2];
        if (!R_FINITE(dy[i])) error("Non-finite derivative encountered by ODE solver.");
    }
}
static const R_CMethodDef cMethods[] = {
    {"cs_initmod", (DL_FUNC)&cs_initmod, 1}, {"cs_initforc", (DL_FUNC)&cs_initforc, 1},
    {"cs_cr", (DL_FUNC)&cs_cr, 6}, {"cs_lv", (DL_FUNC)&cs_lv, 6}, {NULL, NULL, 0}
};
static const R_CallMethodDef callMethods[] = {
    {"cs_prepare", (DL_FUNC)&cs_prepare, 1}, {"cs_trials", (DL_FUNC)&cs_trials, 0}, {NULL, NULL, 0}
};
void R_init_community_simulator(DllInfo *dll) {
    R_registerRoutines(dll, cMethods, callMethods, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
