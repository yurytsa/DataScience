import numpy

class DataframeNumSummarizer():
    def summary (df):
        for colname, coltype in df.dtypes.iteritems():
            if (coltype=="int64"):
                print("Column " + colname + ":")
                print("the minimum value is " + str(numpy.min(df[colname])))
                print("the mean value is " + str(numpy.mean(df[colname])))
                print("the median value is "  + str(numpy.median(df[colname])))
                print("the maximum value is " + str(numpy.max(df[colname])))