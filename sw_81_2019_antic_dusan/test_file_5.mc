//Test 2. zadatka
//testiranje 2 ugnjezdene for petlje

int main()
{
	int zbir = 0;
	int razlika = 0;
	for(int i = 3 : -3){
	zbir = zbir + i;
	razlika = razlika - i;
	for(int j = 5 : 1){
	razlika = zbir - j;
	}
	}
	return 1;
}
