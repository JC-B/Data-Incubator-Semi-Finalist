setwd("C:/Users/denjckb/Downloads")
data.assess <- read.csv("Historic_Secured_Property_Tax_Rolls.csv")
#Answer1
most_common_class <- sort(table(data.assess$Property.Class.Code), decreasing = T)[1]
total_assess_class <- NROW(data.assess$Property.Class.Code)
frac_assess_common_class <- most_common_class/total_assess_class
print(frac_assess_common_class) 
#Answer2
install.packages("data.table")
library(data.table)
latest.positive.assess <- setDT(data.assess["Closed.Roll.Assessed.Improvement.Value" != 0])[,.SD[which.max(Closed.Roll.Fiscal.Year)],by="Block.and.Lot.Number"]
median_latest_positive_assess <- median(latest.positive.assess$Closed.Roll.Assessed.Improvement.Value, na.rm = TRUE)
print(median_latest_positive_assess)
#Answer3
mean_positive_assess_ngbrhd <- aggregate(latest.positive.assess$Closed.Roll.Assessed.Improvement.Value, by=list(latest.positive.assess$Neighborhood.Code),FUN=mean)[2]
diff_max_min_max_mean_positive_assess_ngbrhd <- max(mean_positive_assess_ngbrhd) - min(mean_positive_assess_ngbrhd)
print(diff_max_min_max_mean_positive_assess_ngbrhd)
#Answer6
built_years <- as.factor(latest.positive.assess$Year.Property.Built)
levels(closed_years)
earliest.with.units.valid.year.assess <- setDT(subset(data.assess, data.assess$Year.Property.Built > 999 & data.assess$Year.Property.Built < 2016 & data.assess$Number.of.Units > 0))[,.SD[which.min(Closed.Roll.Fiscal.Year)],by="Block.and.Lot.Number"]
avg_units_b4_1950 <- mean(earliest.with.units.valid.year.assess[Year.Property.Built < 1950, Number.of.Units], na.rm = TRUE)
avg_units_aft_1950 <- mean(earliest.with.units.valid.year.assess[Year.Property.Built > 1949, Number.of.Units], na.rm = TRUE)
diff_units_b4_aft_1950 = abs(avg_units_b4_1950-avg_units_aft_1950)
print(diff_units_b4_aft_1950)
