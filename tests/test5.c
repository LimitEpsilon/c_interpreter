struct ltag1; struct ltag2; struct ltag3; struct ltag4; struct ltag5; struct ltag6; struct ltag7;
void write_int(int x);
void write_string(char *x);

int global_e;
int global_f;

int func1(int dummy)
{
    if(!global_f){
        global_f = 1;
    }

    if(!global_e && global_f > 1){
        return global_e++;
    }
    else if(global_e <= 20/2 && global_f > 3 || 
            global_e <= 10 && global_f > 2){
        global_e = global_e + 3;
        if(global_e == 11-1){
            global_e = global_e + 2;
        }
        else{
            global_e--;
        }
        return (global_e = global_e+1);
    }
    else if(global_f > 3){
        int a;
        a = global_e;
        return (global_e++ % 3) + global_f - (a/10)  + 1 ;
    }
    else{
        global_f++;
        return 30;
    }
}

int func2(int *a, int* b, int c)
{
    int d;
    
    if(c){
        c = c - 2;
        a++;
        d = func2(&((*a)), &b[-1], d=c+1);
        a--;
    }
    else
        d = 0;
    
    a[0] = *(&(*(b+1))-1) = d;

    return func1(0);
}

int func3(int dummy)
{
    int i;
	int *a;
	int b[4];
	int *d;
    
    a = b;
    for(i=0; i<4; i++)
      b[i] = func1(0);
    
    d = &(*a++);
    /* write your own writing function here */
    write_int(*a);
    write_string(",");
    write_int(*d);
    write_string("\n");

    (*d++)++;
    /* write your own writing function here */
    write_int(*(d-1));
    write_string(",");
    write_int(*d);
    write_string("\n");
    
    a = a + 2; /* ((a+1)-(a-1)); */
    /* write your own writing function here */
    write_int(*a);
    write_string("\n");

    (*(*(&d))++)++;
    /* write your own writing function here */
    write_int(*(d-1));
    write_string(",");
    write_int(*d);
    write_string("\n");

    *b = 1;
    b[b[0]-1+*b] = 4;
    a = d = b;
    *(&(*(b+2))) = *(d+b[*a++]-*b);
    /* write your own writing function here */
    write_int(a[-1]);
    write_string(",");
    write_int(*a);
    write_string(",");
    write_int(*(a+1));
    write_string("\n");

    return dummy;
}

struct ltag7{
    int b;
    struct ltag8 {
        int *a; 
    } a[3];
    int c[10];
	int *d;
};

struct ltag4{
    char a;
    struct ltag7 b[3];
    int *c;
};

struct ltag5{
    char a[3];
	char b[2];
	char *c;
    struct ltag7 d;
    int e[8];
	int f[3];
	int g;
};

struct ltag1{
    char *a;
	char b[10];
    struct ltag4 c[2];
    int d[10];
	int e[5];
	int *f;
};

struct ltag2{
    struct ltag4 a[10];
    int b[4];
	int *c;
    struct ltag5 d;
};

struct ltag3{
    int a;
	int b[2];
	int *c;
    struct ltag5 d[7];
    char *e;
	int f[10];
    struct ltag6 {
        struct ltag1 a[10];
        struct ltag2 b;
    } g[3];
    int *h;
	int i;
};

struct ltag1 global_a[10];

int *func5(int *a, int b)
{
    if(b == 0)
      return a;
    (*a) = *a + 2;
    *a = *a - 1;

    return func5(a, b-1); 
}

int func6(int *a, int *b)
{
    int temp;
    int dummy;

    temp = *a;
    *a = *b;
    *b = temp;

    return dummy;
}

int *func7(int *a, int b)
{
    int i;
	int j;
	int k;

    int o;
	int p;

    if(a[0] == 1){
        int m;
		int n;

        for(k=0; k<10; k++){
            global_a[k].c[*a].b[k-k].c[0] = -k;
	    i=0; 
            for(j=10; i != 30 && j <=10 || j == 0 ; i++){
                if(i == j-10) continue;
                m = n = 1;
                
                global_a[*func5(&m, k)-1].c[1].b[0].c[*func5(&n, i)-1] = 
                    10*k + i;
                
                if(i == j-1) break;
            }
        }
        (*a)++;
        return a;
    }
    else if(*a == 2){
        struct ltag3 c[10];

        for(i=0; i<10; i++)
          for(j=0; j<*a+8; j++)
            c[i].g[*a].a[j].d[*a+1] = global_a[i].c[1].b[0].c[j];

        for(i=0; i<10; i++)
          for(j=0; j<10; j++)
            global_a[i].c[1].b[0].c[j] = global_a[i].c[1].b[0].c[j] + c[i].g[*a].a[j].d[*a+1];

        (*a)++;
        return func7(a, b=b+2);
    }
    else if(*a == 3){
        int j;
		int k;
        for(j=b; j!=0; j--)
          for(k=0; k<j; k++)
            if(global_a[0].c[1].b[0].c[k] < 
               global_a[0].c[1].b[0].c[k+1])
              func6(&global_a[0].c[1].b[0].c[k] ,
                    &global_a[0].c[1].b[0].c[k+1]);

        (*a)++;
        return a;
    }

    /* cannot reach here */
    (*a) = 10;
    return a;
}

int main()
{
    int i;
	int j;
	int c[30];
	int d[30];
	int n;
	int l;

    global_e = global_f = 0;

    for(i=0; i<30; i++){
        c[i] = d[i] = 0;
    }

    func2(c, &d[29], func1(0)+func1(0)-1);

    /* write your own writing function here */
    for(i=0; i<30; i++){
        write_int(c[i]);
        write_string(",");
    }
    write_string("\n");
 
    for(i=0; i<30; i++){
        write_int(d[i]);
        write_string(",");
    }
    write_string("\n");

    func3(0);

    for(i=0; i<10; i++)
      for(j=0; j<10; j++)
        global_a[i].c[1].b[0].c[j] = global_a[i].c[1].b[0].c[j] - global_a[i].c[1].b[0].c[j];


    n = l = 1;
    l = *func7(func7(&n, 0), l = l + 7);
    
    /* write your own writing function here */
    for(i=0; i<10; i++){
        for(j=0; j<10; j++){
            write_int(global_a[i].c[1].b[0].c[j]);
            write_string(",");
        }
        write_string("\n");
    }
    write_int(l);
    write_string("\n");
}
