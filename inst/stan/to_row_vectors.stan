functions {
    array[] row_vector to_row_vectors(matrix m) {
        array[rows(m)] row_vector[cols(m)]  av;

        for (i in 1:rows(m)) {
            av[i] = m[i, :];
        }

        return(av);
    }
}
