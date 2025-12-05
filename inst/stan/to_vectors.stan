functions {
    array[] vector to_vectors(matrix m) {
        array[cols(m)] vector[rows(m)]  av;

        for (i in 1:cols(m)) {
            av[i] = m[:, i];
        }

        return(av);
    }
}
