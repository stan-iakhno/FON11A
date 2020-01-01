library("optparse")
option_list <- list(
    make_option(c("-v", "--verbose"), action="store_true", default=TRUE,
        help="Print extra output [default]"),
    make_option(c("-p", "--pattern"),type="character"),
    make_option(c("-c", "--count"), type="integer", default=5,
        help="Number of random normals to generate [default %default]",
        metavar="number")
    )

parse_args(OptionParser(option_list=option_list))

a = count

b = a + 100
print(b)
print(pattern)
