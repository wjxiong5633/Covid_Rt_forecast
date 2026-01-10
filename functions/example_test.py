from darts.datasets import AirPassengersDataset
from darts.models import AutoARIMA
from darts.utils.timeseries_generation import holidays_timeseries
series = AirPassengersDataset().load()
# optionally, use some future covariates; e.g. the value of the month encoded as a sine and cosine series
future_cov = datetime_attribute_timeseries(series, "month", cyclic=True, add_length=6)
# define some boundaries for the parameters
model = AutoARIMA(start_p=8, max_p=12, start_q=1)
model.fit(series, future_covariates=future_cov)
pred = model.predict(6, future_covariates=future_cov)