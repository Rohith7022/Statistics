
# IPL Player Performance and Salary Analysis (Python Version)

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import re
from scipy import stats
import statsmodels.api as sm
import statsmodels.formula.api as smf

# Load and clean IPL data
ipl = pd.read_csv("IPL_ball_by_ball_updated till 2024.csv")
ipl = ipl.dropna(thresh=ipl.shape[1] * 0.5)
ipl = ipl.fillna(0)

# Summarize performance per match
runs = ipl.groupby(['Match.id', 'strikers'])['runs_scored'].sum().reset_index()
runs.rename(columns={'strikers': 'Player', 'runs_scored': 'Total_Runs'}, inplace=True)
wickets = ipl[ipl['wicket_confirmation'] == 1].groupby(['Match.id', 'bowlers']).size().reset_index(name='Total_Wickets')
wickets.rename(columns={'bowlers': 'Player'}, inplace=True)
performance = pd.merge(runs, wickets, on=['Match.id', 'Player'], how='outer').fillna(0)

# Top 3 batsmen and bowlers per season
top_batsmen = ipl.groupby(['season', 'strikers'])['runs_scored'].sum().reset_index().sort_values(['season', 'runs_scored'], ascending=[True, False]).groupby('season').head(3)
top_bowlers = ipl[ipl['wicket_confirmation'] == 1].groupby(['season', 'bowlers']).size().reset_index(name='Total_Wickets').sort_values(['season', 'Total_Wickets'], ascending=[True, False]).groupby('season').head(3)

# Fit distributions for top batsmen
run_counts = top_batsmen['runs_scored']
mu = run_counts.mean()
poisson_fit = stats.poisson(mu)
sns.histplot(run_counts, bins=10, stat='density', color='skyblue')
x = np.arange(0, run_counts.max()+1)
plt.plot(x, poisson_fit.pmf(x), 'r--')
plt.title('Poisson Fit - Top Batsmen Runs')
plt.xlabel('Runs')
plt.ylabel('Density')
plt.show()

# Pathirana distribution fit
pathirana = ipl[ipl['bowlers'].astype(str).str.lower().str.contains('pathirana')]
pathirana_runs = pathirana['runs_scored']
mu_p = pathirana_runs.mean()
fit_poisson = stats.poisson(mu_p)
fit_nb = stats.nbinom.fit(pathirana_runs)
fit_geom = stats.geom.fit(pathirana_runs)
sns.histplot(pathirana_runs, bins=6, stat='density', color='lightgreen')
x = np.arange(0, pathirana_runs.max()+1)
plt.plot(x, stats.poisson.pmf(x, mu_p), 'r-', label='Poisson')
plt.plot(x, stats.nbinom.pmf(x, *fit_nb), 'g--', label='Negative Binomial')
plt.plot(x, stats.geom.pmf(x, *fit_geom), 'b:', label='Geometric')
plt.title("Best Fit Distributions - Pathirana Runs per Ball")
plt.legend()
plt.show()

# Load and clean salary data
salary = pd.read_excel("IPL_SALARIES_2024.xlsx")
def parse_salary(s):
    s = str(s).lower()
    if 'crore' in s:
        return float(re.findall(r"\d+\.?\d*", s)[0]) * 100
    elif 'lakh' in s:
        return float(re.findall(r"\d+\.?\d*", s)[0])
    else:
        try:
            return float(s)
        except:
            return np.nan
salary['Salary_Lakhs'] = salary['Salary'].apply(parse_salary)
salary.dropna(subset=['Salary_Lakhs'], inplace=True)

# Aggregate performance (2022–2024)
recent = ipl[ipl['season'].isin([2022, 2023, 2024])]
perf = recent.groupby('strikers').agg(Total_Runs=('runs_scored', 'sum'), Matches=('Match.id', pd.Series.nunique)).reset_index().rename(columns={'strikers': 'Player'})

# Merge salary with performance
player_data = pd.merge(perf, salary, on='Player', how='inner')

# Correlation and regression
corr = player_data['Total_Runs'].corr(player_data['Salary_Lakhs'])
print(f"Correlation: {corr:.3f}")
model = smf.ols('Salary_Lakhs ~ Total_Runs + Matches', data=player_data).fit()
print(model.summary())

# Outlier detection using Cook’s Distance
influence = model.get_influence()
cooks = influence.cooks_distance[0]
threshold = 4 / len(player_data)
outliers = player_data[cooks > threshold]
cleaned_data = player_data[cooks <= threshold]
model_clean = smf.ols('Salary_Lakhs ~ Total_Runs + Matches', data=cleaned_data).fit()
print(model_clean.summary())

# Visualization
plt.figure(figsize=(10, 6))
sns.scatterplot(data=player_data, x='Total_Runs', y='Salary_Lakhs', hue='Player', legend=False)
sns.regplot(data=player_data, x='Total_Runs', y='Salary_Lakhs', scatter=False, color='red')
plt.title('Player Performance vs Salary')
plt.xlabel('Total Runs (2022–2024)')
plt.ylabel('Salary (in Lakhs)')
plt.show()
