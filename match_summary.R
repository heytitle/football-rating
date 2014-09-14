match_summary <- function(filename) {
    TW_RATIO <- 0.8;
    PS_RATIO <- 1 - TW_RATIO;

    # Team Score
    TMS <- c(0,0);
    names(TMS) <- c( 'TMS1', 'TMS2' );

    # Team Rating
    TR <- c( 0, 0 );


    data <- read.csv(filename);

    ## TEAM SCORE
    for( i in 1:2 ) {
        TMS[i] <- sum(data[ data$Team == i , 'Score' ]);
        TMS[i] <- TMS[i] + sum( data[ data$Team == rival(i), 'OG' ] );
    }

    ## Team Rating
    for( i in 1:2 ) {
        # Diff player factor
        diff_player <- nrow( data[data$Team == i,] );
        diff_player <- diff_player - nrow( data[data$Team == rival(i),] );
        diff_factor <- abs( min( 0, diff_player ) );

        TR[i] <- ( 0.8 * TMS[i] / sum(TMS) + 0.2 * diff_factor );

    }

    rating <- function( row ) {
        PS   <- as.numeric( row['Score'] );
        team <- as.numeric( row['Team'] );
        TMS  <- TMS[team];

        rating <- PS_RATIO * ( PS / TMS);
        rating <- rating + TW_RATIO * TR[team];
        sprintf('%.2f',rating);
    }

    data['Rating'] = apply( data ,1, rating );

    data;

}

rival <- function( i ) {
    i %% 2 + 1;
}

print_result <- function( data ) {
    add_status <- function( row ) {

        if( as.numeric(row['Rating']) == max( data[,'Rating'] ) ) {
            '*'
        }else {
            ''
        }
    }
    data['MoM'] = apply( data ,1, add_status );

    print( data[, c( 'Team', 'Player', 'Rating', 'MoM' ) ] );
}
