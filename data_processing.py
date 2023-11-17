import pandas as pd
import numpy as np

if __name__ == "__main__":
    df = pd.read_csv("./data/MERGED2016_17_PP.csv")
    names = [
        "UNITID",
        "INSTNM",
        "ADMCON7",
        "PREDDEG",
        "CCBASIC",
        "REGION",
        "HBCU",
        "ADM_RATE",
        "SATVRMID",
        "SATWRMID",
        "ACTCMMID",
        "SAT_AVG",
        "DISTANCEONLY",
        "UGDS",
        "NPT4_PUB",
        "NPT4_PRIV",
        "NUM4_PUB",
        "NUM4_PRIV",
        "COSTT4_A",
        "TUITIONFEE_IN",
        "TUITIONFEE_OUT",
        "C150_4",
        "C150_L4",
        "PCTFLOAN",
        "DEBT_MDN",
        "AGE_ENTRY",
        "FEMALE",
        "MARRIED",
        "DEPENDENT",
        "VETERAN",
        "FIRST_GEN",
        "MD_FAMINC",
        "FSEND_1",
        "MD_EARN_WNE_P10",
        "ICLEVEL",
        "OPENADMP",
        "D_PCTPELL_PCTFLOAN",
        "GRADS",
        "TRANS_4_POOLED",
        "C100_4_POOLED",
        "C150_4_PELL",
        "SCHTYPE",
        "PRGMOFR",
        "CIPCODE1",
        "CIPTFBSANNUAL1",
        "MTHCMP1",
        "FTFTPCTPELL",
        "FTFTPCTFLOAN",
        "UG12MN",
        "G12MN",
        "DBRR4_FED_UG_RT",
        "ROOMBOARD_ON",
        "ENDOWBEGIN",
        "MDEARN_PD",
        "COUNT_WNE_1YR",
        "CNTOVER150_1YR",
        "GT_THRESHOLD_P6",
        "MD_EARN_WNE_MALE0_P6",
        "STUFACR",
        "IRPS_WHITE",
        "IRPS_WOMEN",
        "BBRR4_FED_UG_NOPROG",
        "BBRR4_FED_UG_DISCHARGE",
        "ADM_RATE_SUPP"
    ]
    features = df.loc[:, names].set_index("UNITID", drop=True)

    df2 = pd.read_csv("./data/MERGED2017_18_PP.csv")
    t_df = df2.loc[:, ["UNITID", "ADMCON7"]].set_index("UNITID", drop=True)

    df3 = pd.read_csv("./data/MERGED2018_19_PP.csv")
    o_df = df3.loc[:, ["UNITID", "RET_FT4"]].set_index("UNITID", drop=True)

    data = features.join(t_df, how="inner", rsuffix="_2").join(o_df, how="inner")
    indices = data.loc[:, "RET_FT4"].notna() & data.loc[:, "ADMCON7"].notna() & data.loc[:, "ADMCON7_2"].notna()
    data = data[indices]

    data = data.dropna(axis=1, how="all")
    
    data.to_csv("./data/processed_data.csv")