load('../data/usCapitals.rda')
load('../data/crimes.rda')
dat=merge(usCapitals,crimes,by.x='Abbr',by.y='abbr')[,c(1,2,4,5,6,10)]