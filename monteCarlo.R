possible = c();
for(k in 1:1000){
  ratio1 = runif(1, 0, 1);
  ratio2 = 1-ratio1;
  if(ratio1>ratio2){
    result = ratio2/ratio1
  } else {
    result= ratio1/ratio2
  }
  possible[k] = result
}
summary(possible)
