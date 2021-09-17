
gaia_data<-read.csv("GaiaSource-1537485007916299520-1537946871513980544.csv")

gaia_5000pc<-gaia_data[1000/abs(gaia_data$parallax) < 5000 
                       & gaia_data$parallax_over_error>5,]

gaia_1000pc<-gaia_data[1000/abs(gaia_data$parallax) < 1000,]

plot(gaia_5000pc$phot_bp_mean_mag-gaia_5000pc$phot_rp_mean_mag,
               gaia_5000pc$phot_g_mean_mag-5*log10(100/abs(gaia_5000pc$parallax)),
               pch=19, cex=0.25, ylim = c(15,0), 
               main = "Gaia Data Hertzsprung-Russell Diagram 2",
               xlab = "Gaia BP-RP Color", ylab = "Gaia G Absolute Magnitude")

nrow(gaia_5000pc)

distances<-1000/abs(gaia_8000pc$parallax)
magnitudes<-gaia_1000pc$phot_g_mean_mag-5*log10(100/abs(gaia_1000pc$parallax))
hist((magnitudes), breaks=30,
     main = "Distribution of Distance by Magnitude",
     sub = "Within 1000pc",
     xlab = "Absolute Magnitude")
