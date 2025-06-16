## Initialize
setwd("data/dataCleaning/")

## No working code; just a view

## No working code; just a view

## filter age >= one  year
|> filter(.,ageYears>=1)

## correction table
print(corTab)
dat <- (dat
        |> left_join(corTab)
        |> mutate(
          province = ifelse(!is.na(patchProvince), patchProvince, province)
        )
        |> select(-patchProvince)
)
summary(dat |> mutate_if(is.character, as.factor))
