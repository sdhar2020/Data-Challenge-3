import pandas as pd


def calc_cum_percentiles(v, normalize=True):
	"""Calculate the cumulative percentiles of a vector:
	ex:
	15        0.047619
	32        0.619048
	82        0.730159
	108       0.984127
	108108    1.000000
	"""
	if normalize:
		temp = pd.DataFrame(pd.Series(v).value_counts(normalize=True).sort_index().cumsum()).reset_index()
		temp.columns = ['Days_in_Future', 'Bad']
		temp['Healthy'] = 1 - temp['Bad']
		return temp
	else:
		temp = pd.DataFrame(pd.Series(v).value_counts().sort_index().cumsum()).reset_index()
		temp.columns = ['Days_in_Future', 'Bad']
		total = temp['Bad'].iloc[-1]
		temp['Healthy'] = total - temp['Bad']
		return temp