# Heatlhy-Food-Priority-Areas-Dashboard

The Healthy Food Priority Areas Dashboard attempts to make it easy to get the latest data needed to calculate the HFPA score for Pittsburgh census tracts. The app connects to the census API in order to pull in the required metrics to calculate ***Availability**, **Access**,* and ***Utilization***. The tables below outline how each metric is calculated and normalized The resulting HFPA score is the sum of three scores.

# Availability

| GEOID | year | Total_HH | No_Vehicle_Avail | pct_no_veh_avail | no_veh_std | No_Vehicle_Score |
| --- | --- | --- | --- | --- | --- | --- |
| Tract | Year | B08201_001 | B08014_002 | $$\frac{\text{Total HH}}{\text{No Vehicle Avail}}$$ | $$\frac{\text{pct no veh avail}-\frac{\text{sum(no vehicle avail)}}{\text{sum(total HH)}}}{\text{sd(pct no veh avail)}}$$ | $$\text{(no veh std) - min(no veh std)} \times \frac{10}{\text{Max(no veh std)}-\text{Min(no veh std)}}$$ |

| pct_walkable | pct_walkable_std | Walkability_Score |
| --- | --- | --- |
| From Excel Sheet | $$\frac{\text{pct walkable}-\text{Mean(pct walkable)}}{\text{sd(pct walkable)}}$$ | $$\text{(pct walkable std - min(pct walkable std))} \times \frac{10}{\text{max(pct walkable std}-\text{min(pct walkable std)}}$$ |

| Availability |
| --- |
| $$\sqrt{\text{No Vehicle Score} \times \text{Walkability Score}}$$ |

# Access

| GEOID | Year | pop_below_185 | total_pop | pct_below_185 | pct_185_std | Access |
| --- | --- | --- | --- | --- | --- | --- |
| Tract | Year | S1701_C01_041 | S0101_C01_001 | $$\frac{\text{pop below 185}}{\text{total pop}}$$ | $$\frac{\text{pct below 185}-\text{Mean(pct below 185)}}{\text{sd(pct below 185)}}$$ | $$\text{(pct 185 std - MIN(pct 185 std))} \times \frac{10}{\text{MAX(pct 185 std)}-\text{MIN(pct 185 std)}}$$ |

# Utilization

**CDC 500 Cities Data**

| Year | URL |
| --- | --- |
| 2019 | https://chronicdata.cdc.gov/resource/k86t-wghb.csv?placename=Pittsburgh |
| 2018 | https://chronicdata.cdc.gov/resource/k25u-mg9b.csv?placename=Pittsburgh |
| 2017 | https://chronicdata.cdc.gov/resource/kucs-wizg.csv?placename=Pittsburgh |
| 2016 | https://chronicdata.cdc.gov/resource/5mtz-k78d.csv?placename=Pittsburgh |

