# PCA of dummy spectral data
# 13.2.2020
# code source https://learnche.org/pid/latent-variable-modelling/principal-component-analysis/pca-example-analysis-of-spectral-data

library(readr)
spectra <- read.csv("~/Downloads/tablet-spectra.csv", header = FALSE, row.names = 1)
View(tablet_spectra)

# Only extract 4 components, but
# center and scale the data before
# calculation the components
model.pca <- prcomp(spectra,
                    center = TRUE,
                    scale =TRUE,
                    rank. = 4)
summary(model.pca)

spectra.P <- model.pca$rotation
spectra.T <- model.pca$x

# Baseline: mean and standard deviation per column
spectra.mean <- apply(spectra, 2, mean, na.rm=TRUE)
spectra.sd <- apply(spectra, 2, sd, na.rm=TRUE)

# Remove the calculated mean from each column (margin=2)
# by using the subtract function (FUN argument)
spectra.mc <- sweep(spectra, 2, spectra.mean, FUN='-')

# Scale each column, dividing by the standard deviation
spectra.mcuv <- sweep(spectra.mc, 2, spectra.sd, FUN='/')

# Baseline variance
spectra.X2 <- spectra.mcuv * spectra.mcuv

# A = 1
#------
a = 1
spectra.Xhat.a <- spectra.T[,seq(1,a)] %*% t(spectra.P[,seq(1,a)])
spectra.E <- spectra.mcuv - spectra.Xhat.a
spectra.E2 <- spectra.E * spectra.E
spectra.Xhat.a.2 <- spectra.Xhat.a * spectra.Xhat.a

SPE.1 <- sqrt(apply(spectra.E2, 1, sum))
R2.k.a <- apply(spectra.Xhat.a.2, 2, sum) / apply(spectra.X2, 2, sum)

wavelengths <- seq(600, 1898, 2)
plot(wavelengths, R2.k.a, col='darkgreen',
     type='l', lwd=a*2, ylim=c(0,1),
     ylab=expression("R"^2*" per component (wavelength)"),
     xlab="Wavelengths")

# A = 2
#------
a = 2
spectra.Xhat.a <- spectra.T[,seq(1,a)] %*% t(spectra.P[,seq(1,a)])
spectra.E <- spectra.mcuv - spectra.Xhat.a

# mean for each row
spectra.E.mean <- apply(spectra.E, 1, mean, na.rm=TRUE)
spectra.E2 <- spectra.E * spectra.E
spectra.Xhat.a.2 <- spectra.Xhat.a * spectra.Xhat.a

SPE.2 <- sqrt(apply(spectra.E2, 1, sum))
R2.k.a <- apply(spectra.Xhat.a.2, 2, sum) / apply(spectra.X2, 2, sum)

lines(wavelengths, R2.k.a, col='black', type='l', lwd=a*2)


# A = 3
#------
a = 3
spectra.Xhat.a <- spectra.T[,seq(1,a)] %*% t(spectra.P[,seq(1,a)])
spectra.E <- spectra.mcuv - spectra.Xhat.a
spectra.E2 <- spectra.E * spectra.E
spectra.Xhat.a.2 <- spectra.Xhat.a * spectra.Xhat.a

SPE.3 <- sqrt(apply(spectra.E2, 1, sum))
R2.k.a <- apply(spectra.Xhat.a.2, 2, sum) / apply(spectra.X2, 2, sum)

lines(wavelengths, R2.k.a, col='blue', type='l', lwd=a*2)

legend(x=650, y=0.35,
       legend=c(expression("R"^2*": 1st component"),
                expression("R"^2*": 2nd component"),
                expression("R"^2*": 3rd component")),
       col=c("darkgreen", "black", "blue"),
       lty=c(1, 1, 1), lwd=c(2,4,6), cex=1.0)


# SPE plot
N <- dim(spectra)[1]
layout(matrix(c(1,2,3), 3, 1))
plot(seq(1, N), SPE.1, col='darkgreen',
     type='l', lwd=2,  ylab="SPE: A=1",
     ylim=c(0, max(SPE.1)))
plot(seq(1, N), SPE.2, col='black',
     type='l', lwd=2,  ylab="SPE: A=2",
     ylim=c(0, max(SPE.2)))
plot(seq(1, N), SPE.2, col='blue',
     type='l', lwd=2,  ylab="SPE: A=3",
     xlab="Tablet number", ylim=c(0, max(SPE.3)))

