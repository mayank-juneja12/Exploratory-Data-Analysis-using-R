NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

tpm0 <- with(NEI, tapply(Emissions, year, sum))



##Plot 1

png("plot1.png")

plot(names(tpm0), tpm0, type = "l", lwd = 2, xlab = "Year", ylab = expression("Total PM"[2.5]*"Emissions"), col = "red")
abline(v = c(1999, 2002, 2005, 2008), col = "blue", lty = 2, lwd = 4)
title(main = expression("Total US PM"[2.5]*"Emissions by Year"))
axis(1, at = c(1999, 2002, 2005, 2008), labels = paste(c(1999, 2002, 2005, 2008)))

dev.off()     

##Plot 2

library(dplyr)
baltimore <- filter(NEI, fips == "24510") %>%
        group_by(year) %>%
        summarise(sum = sum(Emissions))

png("plot2.png")

plot(baltimore$year, baltimore$sum, type = "l", xlab = "Year", ylab = expression("Total PM"[2.5]*"Emissions"), col = "red")
abline(v = c(1999, 2002, 2005, 2008), col = "blue", lty = 2, lwd = 4)
title(main = expression("Total PM"[2.5]*"Emissions by Year in Baltimore, Maryland"))
axis(1, at = c(1999, 2002, 2005, 2008), labels = paste(c(1999, 2002, 2005, 2008)))

dev.off()

##Plot 3

library(ggplot2)

baltimore1 <- filter(NEI, fips == "24510") %>%
        group_by(year, type) %>%
        summarise(sum = sum(Emissions))

png("plot3.png")

g1 <- ggplot(data = baltimore1, aes(year, sum)) 
g1 + geom_line(aes(color = type)) + labs(title = expression("Total PM"[2.5]*"Emissions by Year and Type in Baltimore, Maryland")) +
        labs(x = "Year", y = expression("Total PM"[2.5]*"Emissions"))

dev.off()

##Plot 4

png("plot4.png")

scc.coal <- SCC[grep("Fuel Comb.*Coal", SCC$EI.Sector), ]
scc0 <- unique(scc.coal$SCC)
nei.coal <- NEI[NEI$SCC %in% scc0, ]

coal0 <- group_by(nei.coal, year, type) %>%
        summarise(sum = sum(Emissions)) 

coal1 <- group_by(nei.coal, year) %>%
        summarise(sum = sum(Emissions)) %>%
        mutate(type = "TOTAL")
        
coal_full <- bind_rows(coal0, coal1)
coal_full$type <- factor(coal_full$type)

g2 <- ggplot(data = coal_full, aes(year, sum))
g2 + geom_line(aes(color = type)) +
        labs(title = expression("Total PM"[2.5]*"Emissions by Coal - Combustion Source")) +
        labs(x = "Year", y = expression("Total PM"[2.5]*"Emissions"))

dev.off()

##Plot 5


png("plot5.png")

scc.vehicles <- SCC[grep("Mobile.*Vehicles", SCC$EI.Sector), ]
scc1 <- unique(scc.vehicles$SCC)
nei.vehicles <- NEI[NEI$SCC %in% scc1, ]

baltimore2 <- filter(nei.vehicles, fips == "24510") %>%
        group_by(year) %>%
        summarise(sum = sum(Emissions)) %>%
        mutate(city = "Baltimore")

g3 <- ggplot(data = baltimore2, aes(year, sum))

g3 + geom_line() +
        labs(title = expression("Total PM"[2.5]*"Emissions by Motor Vehicle Source in Baltimore")) +
        labs(x = "Year", y = expression("Total PM"[2.5]*"Emissions"))

dev.off()

##Plot 6

png("plot6.png")

la0 <- filter(nei.vehicles, fips == "06037") %>%
        group_by(year) %>%
        summarise(sum = sum(Emissions)) %>%
        mutate(city = "Los Angeles")

com_bl <- bind_rows(baltimore2, la0)

g4 <- ggplot(data = com_bl, aes(year, sum))

g4 + geom_line(aes(color = city)) +
        labs(title = expression("Comparison of Total PM"[2.5]*"Emissions by Motor Vehicle Source in Baltimore And Los Angeles")) +
        labs(x = "Year", y = expression("Total PM"[2.5]*"Emissions"))

dev.off()
