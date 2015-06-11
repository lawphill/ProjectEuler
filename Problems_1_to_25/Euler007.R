# Find the 10001st prime number

max_num <- 999999
nums <- 1:max_num
nums[1] <- 0
to_find <- 10001
found<-0
for(i in 2:floor(max_num/2)){
  if(nums[i] != 0){
    nums[seq.int(i*2,max_num,i)] <- 0
    found <- found + 1
    if(found == to_find){
      print(i)
      break
    }
  }
}