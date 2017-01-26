library(ggplot2)
library(plyr)

domains <- read.csv("~/Downloads/nyc_Domain_Registrations (2).csv", 
                    stringsAsFactors=FALSE)
domains$Nexus.Category <- NULL
names(domains) <- c("name", "date")
domains$date <- strptime(domains$date, format = "%m/%d/%Y %H:%M:%S")

domains$length <- nchar(domains$name)-4

bbi<-element_text(face="bold.italic", color="black")

length_count <- ggplot(data=domains, aes(x=length)) + 
  geom_histogram(binwidth=1, col="black", fill="light blue") +
  scale_x_continuous(limits=c(0,26), breaks = 1:25) +
  labs(y="Count", x = "Domain Length", title = ".nyc Domains by Length") +
  theme(title=bbi)

freq <- count(domains, 'length')[3:25,]

freq$pool <- 36^freq$length
freq$sat <- freq$freq/freq$pool

sat_count <- ggplot(data=freq, aes(x=length, y=sat)) + geom_bar(stat="identity", fill="light blue", col="black") +
  scale_y_continuous(breaks=c(0,0.02,0.04,0.06,0.08), 
                     labels=c("0%","2%","4%","6%","8%")) +
  scale_x_continuous(breaks=3:25) +
  labs(y="Saturation", x = "Domain Length", title = ".nyc Domain Saturation by Length") +
  theme(title=bbi)

#picks row with October mass release date for start
attach(domains)
domains <- domains[order(domains$date), ]
start <- domains[7200,2]
domains.time <- domains[domains$date > start,]

time_length <- ggplot(domains.time, aes(x=date, y=length)) + 
  geom_point(color="steel blue", alpha=0.02) +
  labs(y="Domain Length", x = "Date", title = ".nyc Domain purchases \nby date and length") +
  theme(title=bbi)

topwords <- read.csv("~/Desktop/topwords_2.csv", stringsAsFactors=FALSE)
topwords$domain <- paste(topwords$word,".nyc", sep="")
domains.time$name <- tolower(domains.time$name)

domains.top <- domains.time[domains.time$name %in% topwords$domain,]
dim(domains.top)
names(domains.top) <- c("domain", "date", "length")

domains.top <- merge(domains.top, topwords)


rank_date <- ggplot(domains.top, aes(x=date, y=rank)) + 
  geom_point(color = "steelblue") +
  scale_y_log10() +
  geom_text(data=subset(domains.top, rank<100), 
            aes(y=rank, label=domain), size=4,
            position = position_nudge(x=3500000)) +
  labs(y="Date", x = "Word rank", title = ".nyc Domain purchases \nby date and word rank") +
  theme(title=bbi)
