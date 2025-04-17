### Lab 12 Assignment:

# Summary statistics of phosphorus in soja98 data

data(soja98)           
help(soja98)           
str(soja98)

mean(soja98$P)
median(soja98$P)
min(soja98$P)
max(soja98$P)

# Hotspot map of phosporus in soja98 data
### Step 1: Select Relevant Columns

soja98 <- soja98[, c(1, 2, 3)]  
head(soja98, 3)

### Step 2: Convert to Spatial Points

coordinates(soja98) <- ~X+Y  

### Step 3: Split Data into Calibration and Validation Sets

set.seed(4887)                             
IDX <- sample(1:nrow(soja98), 150)         

cal <- soja98[IDX, ]                       
val <- soja98[-IDX, ]                      

### Step 4: Inverse Distance Weighting (IDW)

### Create a Grid for Interpolation

cellSize <- 5  
range.all <- extent(soja98)  

grd <- expand.grid(
  x = seq(range.all[1], range.all[2], by = cellSize),  
  y = seq(range.all[3], range.all[4], by = cellSize)   
)
coordinates(grd) <- ~x + y   
gridded(grd) <- TRUE         

### Perform IDW Interpolation

P.idw <- idw(P ~ 1, cal, grd)  

# Step 5: Ordinary Kriging 

variog <- variogram(P ~ 1,cal)  
sphr.fit <- fit.variogram(variog, vgm("Sph"))  
exp.fit <- fit.variogram(variog, vgm("Exp"))  

### Performing Kriging

P.sph <- krige(P~1, cal, grd, model = sphr.fit)  
P.exp <- krige(P~1, cal, grd, model = exp.fit)  

### Visualize Kriging Results

spplot(P.sph, main = "Ordinary Kriging (Spherical)")
spplot(P.exp, main = "Ordinary Kriging (Exponential)")

