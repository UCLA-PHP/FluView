dataset_prelab = tryCatch(
  expr = {
    temp1 = iris
    # stop()
    message('CDC is Working')
    return(temp1)
  },
  error = function(e){
    temp2 = mtcars
    message('CDC is Not Available, Using Backed Up Data')
    print(e)
    return(temp2)
  }

)

print("goodbye")
