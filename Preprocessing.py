import pandas as pd
import numpy as np
from sklearn.preprocessing import StandardScaler
from statsmodels.stats.outliers_influence import variance_inflation_factor
from statsmodels.tools.tools import add_constant
from scipy.stats import zscore
from matplotlib import pyplot as plt

GROUP_VAR = ['cntry']
INDEP_VARS = ['agea', 'gndr', 'hinctnta', 'eisced', 'rlgdgr']
DENOM_VAR = ['rlgdnm']
INDIVID_RELIG_VARS = ['Roman_Catholic', 'Protestant', 'Eastern_Orthodox', 'Other_Christian_denomination', 'Jewish',
                      'Islam', 'Eastern_religions', 'Other_Non_Christian_religions', 'Non_Confessional']
INDIVID_RELIG_CODE_DICT = {code: var for var, code in
                           zip(INDIVID_RELIG_VARS, list(range(1, len(INDIVID_RELIG_VARS))) + [66])}
CHANGE_DICT = {'AT': 'Австрия', 'BE': 'Бельгия', 'BG': 'Болгария', 'CH': 'Швейцария', 'CY': 'Кипр', 'CZ': 'Чехия',
               'DE': 'Германия', 'DK': 'Дания', 'EE': 'Эстония', 'ES': 'Испания', 'FI': 'Финляндия', 'FR': 'Франция',
               'GB': 'Великобритания', 'GR': 'Греция', 'HR': 'Хорватия', 'HU': 'Венгрия', 'IE': 'Ирландия',
               'IS': 'Исландия', 'IT': 'Италия', 'LT': 'Литва', 'LV': 'Латвия', 'ME': 'Черногория', 'MK': 'Македония',
               'NL': 'Нидерланды', 'NO': 'Норвегия','PL': 'Польша', 'PT': 'Португалия', 'RS': 'Сербия', 'SE': 'Швеция',
               'SI': 'Словения', 'SK': 'Словакия'}
MARGINALS = ['IL', 'RU', 'TR']

DEP_VARS = ['imsmetn', 'imdfetn']
pd.set_option('display.max_columns', None)

# Фильтрация недопустимых и пропущенных значений
df = pd.read_csv('./ESS10.csv', low_memory=False)[GROUP_VAR + INDEP_VARS + DEP_VARS + DENOM_VAR]
interm = pd.read_csv('./ESS10SC.csv', low_memory=False)[GROUP_VAR + INDEP_VARS + DEP_VARS + DENOM_VAR]
df = pd.concat([df, interm])
df = df[df['agea'] != 999]
df = df[df['gndr'] <= 2]
df = df[df['hinctnta'] <= 10]
df = df[(df['eisced'] > 0) & (df['eisced'] <= 7)]
df = df[df['rlgdnm'] <= 66]
df = df[df['rlgdgr'] <= 10]
df = df[(df[['imsmetn', 'imdfetn']] <= 4).all(axis=1)]
df['imdfetn'] = df['imdfetn'].astype('int')
df['imsmetn'] = df['imsmetn'].astype('int')
df.reset_index(inplace=True, drop=True)
df['cntry'].replace(CHANGE_DICT, inplace=True)
df = df[~df['cntry'].isin(MARGINALS)]

df['rlgdnm'] = df['rlgdnm'].replace(INDIVID_RELIG_CODE_DICT)

df = df[df['rlgdnm'].isin(['Roman_Catholic', 'Protestant', 'Eastern_Orthodox', 'Islam'])]
df.info()
print((df['eisced'].value_counts()))
df['eisced'] = df['eisced'].map(lambda x: 1 if x >= 6 else 0)
print((df['eisced'].value_counts()))

# Стандартизация
# df[INDEP_VARS] = df.groupby('cntry')[INDEP_VARS].transform(lambda x: (x - x.mean()))
# print('Коэффициенты корреляции:', df.corr(numeric_only=True)[DEP_VARS], sep='\n')
scale_vars = ['agea', 'hinctnta', 'rlgdgr']
df[scale_vars] = StandardScaler().fit_transform(df[scale_vars])

# Вычисление VIF и удаление дубликатов
vif_data = pd.DataFrame()
X_with_const = add_constant(df[INDEP_VARS])
vif_data['Переменная'] = X_with_const.columns
vif_data['VIF'] = [round(variance_inflation_factor(X_with_const.values, i), 2) for i in range(X_with_const.shape[1])]
vif_data.to_csv('VIF.csv', index=False)

print('Количество дубликатов до удаления:', df.duplicated().sum())
df.drop_duplicates(keep='first', inplace=True)
print('Количество дубликатов после удаления:', df.duplicated().sum())
print('Количество респондентов по странам:', df.groupby(GROUP_VAR)[GROUP_VAR].count(), sep='\n')
print('Количество стран:', df['cntry'].nunique())
print('Итоговый размер выборки:', df.shape[0])

txt = ['«Разрешить въезд многим/немногим иммигрантам\nтой же расы/этнической группы, что и большинство» (imsmetn)',
       '«Разрешить въезд многим/немногим иммигрантам\nрасы/этнической группы, отличной от большинства» (imdfetn)']

# Вывод распределения зависимых переменных
for DV, t in zip(DEP_VARS[-2:], txt):
    print(df[DV])
    df[DV].value_counts().sort_index().plot.bar()
    plt.title(f'Распределение переменной {t}')
    plt.xlabel(DV)
    plt.ylabel('Частота')
    plt.show()

df.to_csv('dataset.csv', index=False)
