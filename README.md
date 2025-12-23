Diversification is essential to portfolio management, aiming to reduce risk by investing across various weakly/negatively correlated assets. Value at risk is not a coherent risk
measure as it can violate subadditivity meaning the potential losses on a portfolio may exceed the sum of its individual asset’s losses. This violation can lead to misleading 
conclusions about the benefits of diversification, especially during market stress (periods of non-linear dependence) when asset correlations increase. This project explores how to
effectively provide a practical recommendation to project better risk estimates during high volatility/fewer stable periods (reducing risk doesn’t necessarily mean reduction in potential
losses in monetary terms).

In this project we estimate Value at Risk (VaR) for two individual stocks (MSFT and COST) using Historical Simulation, EWMA, GARCH, and t-GARCH models. We statistically 
backtest each model using VaR violations and likelihood-ratio tests to assess coverage accuracy and tail-risk performance. The analysis is then extended to portfolio VaR, 
combining the two stocks and estimating risk using, HS, DCC-EWMA and DCC-GARCH, followed by backtesting to evaluate diversification and time-varying dependence effects.
