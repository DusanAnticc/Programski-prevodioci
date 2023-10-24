//OPIS: for petlja dva puta ista lokalna prom
//RETURN: 14

int main() {
  int b;
  int k;
  k=0;
  b=0;
  for(int a = 5 : 1 ){
  	b = b + a;
  }
  
  for(int a = 5 : 1 ){
  	k = k + a;
  }
  return k;
}
