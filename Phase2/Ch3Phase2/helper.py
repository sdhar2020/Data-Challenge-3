import pandas as pd


def calc_cum_percentiles(v):
	"""Calculate the cumulative percentiles of a vector:
	ex:
	15        0.047619
	32        0.619048
	82        0.730159
	108       0.984127
	108108    1.000000
	"""
	temp = pd.DataFrame(pd.Series(v).value_counts(normalize=True).sort_index().cumsum()).reset_index()
	temp.columns = ['Days_in_Future', 'PercentageBad']
	temp['PercentHealthy'] = 1 - temp['PercentageBad']
	return temp