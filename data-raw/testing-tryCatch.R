## code to prepare `testing tryCatch` dataset goes here
dataset_prelab = tryCatch(
  expr = {
    temp1 = iris
    message('CDC is Working')
    temp1
  },
  error = function(e){
    temp2 = mtcars
    message('CDC is Not Available, Using Backed Up Data')
    print(e)
    temp2
  }

)

class(dataset_prelab)
