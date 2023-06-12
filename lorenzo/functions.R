### Tutorial on Writing Functions


  choose_color <- function(n=1, tf = TRUE) {
    
   x <- sample(colors(),n, replace = tf)
    
   return(x)
   
  }

  
  choose_color()  
  

  
  choose_color(3)  
  

  choose_color(3, tf=F)  
  

  
  choose_color(30)  
  
  any(duplicated(  choose_color(30)  ))
  
  
  
  choose_color <- function(n=1, tf = TRUE) {
    
    x <- sample(colors(),n, replace = tf)
    
    res <- any(duplicated(x))
    
    return(list(x, res))
    
  }
  
  
  choose_color(30)
  
  
  
  
  choose_color <- function(n=1, tf = TRUE) {
    
    x <- sample(colors(),n, replace = tf)
    
    res <- any(duplicated(x))
  
    if(res==F) {
      
    return(list(x, res))
    
    } else {
    
    res1 <- x[duplicated(x)]  
        
    return(list(x, res, res1))
      
      
    }
      
      
  }
  
  choose_color(30)
  
  
  
  
  
  
  