{
    'whileTest_ZeroToFour_PrintsZeroToFour': (
        '''
    func print_int(x);
    
    func main() {
    i <- 0;
    while i != 5 {
    print_int(i);
    i <- i + 1;
    }
    }        
    ''',
        '0\n1\n2\n3\n4\n'
    ),

    'whileTest_OneToFive_PrintsOneToFive': (
        '''
    func print_int(x);
    
    func main() {
    i <- 0;
    while i != 5 {
    i <- i + 1;
    print_int(i);
    }
    }        
    ''',
        '1\n2\n3\n4\n5\n'
    ),

    'whileTest_OneToFiveWithBrackets_PrintsOneToFive': (
        '''
    func print_int(x);
    
    func main() {
    i <- 0;
    while (i != 5) {
    i <- i + 1;
    print_int(i);
    }
    }        
    ''',
        '1\n2\n3\n4\n5\n'
    ),

    'whileTest_OneToFive_BreaksOnThree': (
        '''
    func print_int(x);
    
    func main() {
    i <- 0;
    while i != 5 {
    i <- i + 1;
    if i == 3 {break;}
    print_int(i);
    }
    }
    ''',
        '0\n1\n2\n'
    ),

    'whileTest_OneToFiveWithLabel_BreaksOnThree': (
        '''
    func print_int(x);
    
    func main() {
    i <- 0;
    
    main_loop:
    while i != 5 {
    i <- i + 1;
    if (i == 3) {break main_loop;}
    print_int(i);
    }
    }
    ''',
        '0\n1\n2\n'
    ),

    'whileTest_OneToFiveNoLabel_ContinuesOnThree': (
        '''
    
    '''
    )}
