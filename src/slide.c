#include <R.h>
#include <math.h>
#include <string.h>

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
