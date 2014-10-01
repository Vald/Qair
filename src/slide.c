#include <R.h>
#include <math.h>
#include <string.h>

#include <Rinternals.h>

SEXP test(SEXP args)
{
	PROTECT_INDEX px;

	SEXP rho, X, XX, FUN, idx, tmp, R_fcall;
	int width, step, id0, idn, lans, k;
	rho = CADR(args);
	PROTECT_WITH_INDEX(X=CADDR(args), &px);
	XX = PROTECT(eval(CADDR(args), rho));
	width = asInteger(CADDDR(args));
	step = asInteger(CAD4R(args));
	FUN = CAD4R(CDR(args));
	R_xlen_t n = xlength(XX);  // a vector, so will be valid.
	Rboolean realIndx = n > INT_MAX;
	lans = (int) ceil((double) n / (double) step);

	SEXP ans = PROTECT(allocVector(VECSXP, lans));

	for(R_xlen_t i = 0; i < lans; i++) {
		id0 = (i + 1) * step - width;
		idn = id0 + width;
		if(id0 < 0) id0 = 0;
		if(idn > n) idn = n;

		idx = PROTECT(allocVector(realIndx ? REALSXP : INTSXP, idn-id0));

		for(R_xlen_t j = 0; j < idn - id0; j++){
			k  = j+id0+1;
			if (realIndx) REAL(idx)[j] = (double)k;
			else INTEGER(idx)[j] = (int)k;
		}

		if(isVectorAtomic(XX))
			tmp = PROTECT(tmp = LCONS(R_BracketSymbol,
				LCONS(XX, LCONS(idx, R_NilValue))));
		else
			tmp = PROTECT(LCONS(R_BracketSymbol,
				LCONS(X, LCONS(idx, R_NilValue))));
		R_fcall = PROTECT(LCONS(FUN,
				LCONS(tmp, LCONS(R_DotsSymbol, R_NilValue))));

		tmp = eval(R_fcall, rho);

		if (MAYBE_REFERENCED(tmp)) tmp = lazy_duplicate(tmp);
		SET_VECTOR_ELT(ans, i, tmp);
		UNPROTECT(3);
	}


	UNPROTECT(3);
	return ans;
}

double max(double * values, int start, int window, int nbOk )
{
	int i, n = 0;
	double result = *(values + start);
	for(i = start; i < window + start; i++)
	{
		if(!ISNAN(*(values + i)))
		{
			n++;
			if(result < *(values + i))
			{
			result = *(values + i);
			}
		}
	}
	if(n >= nbOk)
	{
		return result;
	}
	else
	{
		return NA_REAL;
	}

}

double min(double * values, int start, int window, int nbOk )
{
	int i, n = 0;
	double result = *(values + start);
	for(i = start; i < window + start; i++)
	{
		if(!ISNAN(*(values + i)))
		{
			n++;
			if(result > *(values + i))
			{
			result = *(values + i);
			}
		}
	}
	if(n >= nbOk)
	{
		return result;
	}
	else
	{
		return NA_REAL;
	}

}

double mean(double * values, int start, int window, int nbOk)
{
	int i, n = 0;
	double result = 0;
	for(i = start; i < window + start; i++)
	{
		if(!ISNAN(*(values + i)))
		{
			n++;
			result += *(values + i);
		}
	}
	if(n >= nbOk)
	{
		return result/n;
	}
	else
	{
		return NA_REAL;
	}
}

double var(double * values, int start, int window, int nbOk)
{
	int i, n = 0;
	double moyenne, result = 0;
	moyenne = mean(values, start, window, nbOk);
	for(i = start; i < window + start; i++)
	{
		if(!ISNAN(*(values + i)))
		{
			n++;
			result += (*(values + i) - moyenne) * (*(values + i) - moyenne);
		}
	}
	if(n >= nbOk)
	{
		return result/(n-1);
	}
	else
	{
		return NA_REAL;
	}
}

void slide(double * values, int * length, int * window, int * step, double * results, int * nbOk, char ** fun)
{
	int i, lengthres, start;
	lengthres = (int) ceil((double) *length / (double) *step);

	if(strcmp(*fun, "mean")==0)
	{
		for(i = 0; i < lengthres; i++)
		{
			start = (i + 1) * *step - *window;
			if(start < 0)
			{
				*(results + i) = mean(values, 0, start + *window, *nbOk);
			}
			else if(start + *window <= *length)
			{
				*(results + i) = mean(values, start, *window, *nbOk);
			}
			else
			{
				*(results + i) = mean(values, start, *length - start, *nbOk);
			}
		}
	}else if(strcmp(*fun, "var")==0)
	{
		for(i = 0; i < lengthres; i++)
		{
			start = (i + 1) * *step - *window;
			if(start < 0)
			{
				*(results + i) = var(values, 0, start + *window, *nbOk);
			}
			else if(start + *window <= *length)
			{
				*(results + i) = var(values, start, *window, *nbOk);
			}
			else
			{
				*(results + i) = var(values, start, *length - start, *nbOk);
			}
		}
	}else if(strcmp(*fun, "max")==0)
	{
		for(i = 0; i < lengthres; i++)
		{
			start = (i + 1) * *step - *window;
			if(start < 0)
			{
				*(results + i) = max(values, 0, start + *window, *nbOk);
			}
			else if(start + *window <= *length)
			{
				*(results + i) = max(values, start, *window, *nbOk);
			}
			else
			{
				*(results + i) = max(values, start, *length - start, *nbOk);
			}
		}
	}else if(strcmp(*fun, "min")==0)
	{
		for(i = 0; i < lengthres; i++)
		{
			start = (i + 1) * *step - *window;
			if(start < 0)
			{
				*(results + i) = min(values, 0, start + *window, *nbOk);
			}
			else if(start + *window <= *length)
			{
				*(results + i) = min(values, start, *window, *nbOk);
			}
			else
			{
				*(results + i) = min(values, start, *length - start, *nbOk);
			}
		}
	}
}
