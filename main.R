install_packages()

transportation <- c(0,1,7,8,9)

energy_sectors <- c(2,3,4,6)

water_sectors <- c(5,10,11)

#transportation
PPP = read.csv('PPP data Encoded.csv')    

#Everything
PPP_full = read.csv('Encoded FUll Data.csv')

energy_PPP <- PPP_full[PPP_full$Sector %in% energy_sectors, ]

water_PPP <- read.csv('Water PPP encoded.csv')