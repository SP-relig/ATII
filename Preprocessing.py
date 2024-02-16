import pandas as pd
import numpy as np
from statsmodels.stats.outliers_influence import variance_inflation_factor
from statsmodels.tools.tools import add_constant
from scipy.stats import zscore
from matplotlib import pyplot as plt

GROUP_VAR = ['cntry']
INDEP_VARS = ['agea', 'gndr', 'hinctnta', 'eisced', 'rlgdnm', 'rlgdgr']
INDIVID_RELIG_VARS = ['Roman_Catholic', 'Protestant', 'Eastern_Orthodox', 'Other_Christian_denomination', 'Jewish',
                      'Islam', 'Eastern_religions', 'Other_Non_Christian_religions', 'Non_Confessional']
INDIVID_RELIG_CODE_DICT = {code: var for var, code in
                           zip(INDIVID_RELIG_VARS, list(range(1, len(INDIVID_RELIG_VARS))) + [66])}
CHANGE_DICT = {'AT': 'Австрия', 'BE': 'Бельгия', 'BG': 'Болгария', 'CH': 'Швейцария', 'CY': 'Кипр', 'CZ': 'Чехия',
               'DE': 'Германия', 'DK': 'Дания', 'EE': 'Эстония', 'ES': 'Испания', 'FI': 'Финляндия', 'FR': 'Франция',
               'GB': 'Великобритания', 'HR': 'Хорватия', 'IE': 'Ирландия', 'IS': 'Исландия', 'IT': 'Италия',
               'LT': 'Литва', 'LV': 'Латвия', 'ME': 'Черногория', 'NL': 'Нидерланды', 'NO': 'Норвегия',
               'PL': 'Польша', 'PT': 'Португалия', 'RS': 'Сербия', 'SE': 'Швеция', 'SI': 'Словения', 'SK': 'Словакия'}

DEP_VARS = ['imueclt', 'imwbcnt', 'imsmetn', 'imdfetn']
pd.set_option('display.max_columns', None)

# Фильтрация недопустимых и пропущенных значений
df = pd.read_csv('./ESS9e03_1.csv', low_memory=False)[GROUP_VAR + INDEP_VARS + DEP_VARS]
df = df[df['agea'] != 999]
df = df[df['gndr'] <= 2]
df = df[df['hinctnta'] <= 10]
df = df[(df['eisced'] > 0) & (df['eisced'] <= 7)]
df = df[df['rlgdnm'] <= 66]
df = df[df['rlgdgr'] <= 10]
df = df[(df[['imsmetn', 'imdfetn']] <= 4).all(axis=1)]
df = df[(df[['imueclt', 'imwbcnt']] <= 10).all(axis=1)]
df['imdfetn'] = df['imdfetn'].astype('int')
df['imsmetn'] = df['imsmetn'].astype('int')
df.reset_index(inplace=True, drop=True)
df['cntry'].replace(CHANGE_DICT, inplace=True)

# Вычисление личной религиозности
df['rlgdnm'] = df['rlgdnm'].replace(INDIVID_RELIG_CODE_DICT)
enc_array = [[row['rlgdgr'] if name == row['rlgdnm'] else 0 for name in INDIVID_RELIG_VARS] for
             (_, row) in df.iterrows()]
df[INDIVID_RELIG_VARS] = np.array(enc_array)

# Создание групповых переменных
group_relig_names = ['Protestant', 'Eastern_Orthodox']
group_relig_names = ['Dom_' + gr for gr in group_relig_names]
state_rel = df.groupby('cntry')['rlgdnm'].transform(lambda x: 'Dom_' + x.mode()[0])
df = df.join(pd.get_dummies(state_rel)[group_relig_names])

group_relig_names.append('Mean_rel')
df['Mean_rel'] = df.groupby('cntry')['rlgdgr'].transform('mean')
mean_rel = df.groupby('cntry')['Mean_rel'].mean().sort_values()
print(mean_rel)
print(mean_rel.describe())

rel_per = df.groupby('cntry')['rlgdnm'].agg(pd.Series.mode)
print(rel_per.to_string(index=False))
print(rel_per.index.to_series().to_string(index=False))

# Вычисление преобладающей религии по странам
count_rel = df[df['rlgdnm'].isin(['Roman_Catholic', 'Protestant', 'Eastern_Orthodox', 'Islam'])].groupby(['cntry', 'rlgdnm'])['rlgdnm'].count().sort_index()
print(count_rel)

# Проверка числа людей в каждой конфессии и удаление лишних религиозных признаков
print('Подсчёт ненулевых значений в каждом столбце:', df.astype(bool).sum(axis=0), sep='\n')
stalin_list = ['Jewish', 'Eastern_religions', 'Other_Non_Christian_religions', 'Other_Christian_denomination']
INDIVID_RELIG_VARS = [i for i in INDIVID_RELIG_VARS if i not in stalin_list]
INDEP_VARS = INDEP_VARS[:-2]
df = df[GROUP_VAR + INDEP_VARS + INDIVID_RELIG_VARS + group_relig_names + DEP_VARS]

# Удаление выбросов
print('Респондентов в датасете до удаления выбросов:', df.shape[0])
denombr_names = INDEP_VARS + DEP_VARS
df[['Z_score_' + name for name in denombr_names]] = df[denombr_names].apply(zscore)
for name in denombr_names:
    df = df[abs(df['Z_score_' + name]) < 3]
df.drop(['Z_score_' + name for name in denombr_names], axis=1, inplace=True)
print('Респондентов в датасете после удаления выбросов:', df.shape[0])


# Центрирование и стандартизация по странам
df[INDEP_VARS] = df.groupby('cntry')[INDEP_VARS].transform(lambda x: (x - x.mean()) / x.std())
print('Коэффициенты корреляции:', df.corr(numeric_only=True)[DEP_VARS], sep='\n')

# Вычисление VIF и удаление дубликатов
vif_data = pd.DataFrame()
X_with_const = add_constant(df[INDEP_VARS + INDIVID_RELIG_VARS + group_relig_names])
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
