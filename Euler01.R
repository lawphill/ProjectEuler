# Calculate sum of all multiples of 3 or 5 under 1000
# Brute force using a list comprehension
l <- 1:999
sum(l[l %% 3 == 0 | l %% 5 == 0])