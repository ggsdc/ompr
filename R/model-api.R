#' Get all unique names of the model variables
#'
#' @param model the model
#' @return a character vector ordered in the same way
#'         as the constraint matrix columns and objective vector
#'
#' @examples
#' library(magrittr)
#' model <- MIPModel() %>%
#'   add_variable(x[i], i = 1:3)
#' variable_keys(model)
#' @export
variable_keys <- function(model) UseMethod("variable_keys")

#' @export
variable_keys.optimization_model <- function(model) {
  if (length(model$variables) == 0) {
    return(character(0))
  }
  sort(unlist(lapply(sort(names(model$variables)),
    function(x) {
      var <- model$variables[[x]]
      if (var$arity > 0) {
        var_codes <- paste0(x, "_", var$instances)
        vapply(var_codes, function(var_code) {
          splited_els <- strsplit(var_code, "_", fixed = TRUE)[[1]]
          paste0(splited_els[1], "[",
                 paste0(splited_els[2:length(splited_els)], collapse = ","),
                 "]")
        }, character(1))
      } else {
        x
      }
    }), use.names = FALSE)
  )
}

# helper function that creates a function
# that can extract coefficients and constants
# out of an expression
build_coefficent_vector_fun <- function(model_var_keys) {
  n_cols <- length(model_var_keys)
  function(extracted_coefficients) {
    coef_vector <- rep.int(0, n_cols)
    coefficients <- extracted_coefficients
    names(coefficients) <- NULL
    bound_coefs <- unlist(Map(function(var_coef) {
      var_ast <- var_coef$ast
      if (is.call(var_ast) && length(var_ast) > 1) {
        var_name <- as.character(var_ast[[2]])
        search_key <- paste0(var_name, "[",
                             paste0(as.character(var_ast[3:length(var_ast)]),
                                    collapse = ","), "]")
      } else {
        var_name <- as.character(var_ast)
        search_key <- var_name
      }
      setNames(var_coef$coef, search_key)
    }, coefficients))
    coef_positions <- match(names(bound_coefs), model_var_keys)
    coef_positions <- coef_positions[!is.na(coef_positions)]
    if (length(coef_positions) > 0) {
      coef_vector[coef_positions] <- as.numeric(bound_coefs)
    }
    coef_vector
  }
}


#' Extract the objective function from a model
#'
#' @param model the model
#'
#' @examples
#' library(magrittr)
#' model <- MIPModel() %>%
#'   add_variable(x[i], i = 1:5) %>%
#'   set_objective(sum_expr(i * x[i], i = 1:5) + 10)
#' objective_function(model)
#' @export
objective_function <- function(model) UseMethod("objective_function")

#' @export
objective_function.optimization_model <- function(model) {
  objective <- model$objective
  has_objective <- !is.null(objective)
  build_coefficent_vector <- build_coefficent_vector_fun(variable_keys(model))
  if (has_objective) {
    coefficients <- extract_coefficients_internal(
      model$objective$expression[[1]])
    obj_constant <- coefficients$constant
    if (!is.numeric(obj_constant)) obj_constant <- 0
    coefficients <- coefficients$coefficients
    names(coefficients) <- NULL
    obj_vector <- build_coefficent_vector(coefficients)
    list(vector = obj_vector, constant = obj_constant)
  } else {
    n_vars <- sum(unlist(nvars(model)))
    list(vector = rep.int(0, n_vars), constant = 0)
  }
}

#' Extract the constraint matrix, the right hand side and the directions from a model
#'
#' @param model the model
#' @return a list with three named elements.
#'         'matrix' is the constraint matrix.
#'         'rhs' is the right hand side vector in the order of the matrix.
#'         'direction' is a vector of the constraint directions
#'
#' @examples
#' library(magrittr)
#' model <- MIPModel() %>%
#'   add_variable(x[i], i = 1:3) %>%
#'   add_variable(y[i], i = 1:3) %>%
#'   add_constraint(x[i] + y[i] <= 1, i = 1:3)
#' extract_constraints(model)
#' @export
extract_constraints <- function(model) UseMethod("extract_constraints")

#' @export
extract_constraints.optimization_model <- function(model) {
  build_coefficent_vector <- build_coefficent_vector_fun(variable_keys(model))
  matrices <- lapply(model$constraints, function(constraint) {
    coefficients_lhs <- extract_coefficients_internal(constraint$lhs[[1]])
    coefficients_rhs <- extract_coefficients_internal(constraint$rhs[[1]])
    direction <- constraint$direction
    list(
      lhs = build_coefficent_vector(coefficients_lhs$coefficients),
      rhs = build_coefficent_vector(coefficients_rhs$coefficients),
      direction = direction,
      lhs_constant = coefficients_lhs$constant,
      rhs_constant = coefficients_rhs$constant
    )
  })

  constraint_matrix <- t(rbind(sapply(matrices, function(constraint) {
    constraint$lhs - constraint$rhs
  })))

  # build row upper bound (aka b)
  constraint_rhs <- vapply(matrices, function(constraint) {
    constraint$rhs_constant - constraint$lhs_constant
  }, numeric(1))

  constraint_dir <- vapply(matrices, function(constraint) {
    constraint$direction
  }, character(1))

  list(
    matrix = constraint_matrix,
    direction = constraint_dir,
    rhs = constraint_rhs
  )
}

#' Number of variables of a model
#'
#' @param model the model
#' @return a list with three named elements.
#'         'binary' => number of binary variables,
#'         'integer' => number of integer variables,
#'         'continuous' => number of continuous variables.
#'
#' @examples
#' library(magrittr)
#' model <- MIPModel() %>%
#'   add_variable(x[i], i = 1:10, type = "binary") %>%
#'   add_variable(y[i], i = 1:5, type = "continuous") %>%
#'   add_variable(z[i], i = 1:2, type = "integer")
#' nvars(model)
#' @export
nvars <- function(model) UseMethod("nvars")

#' @export
nvars.optimization_model <- function(model) {
  stopifnot(is.list(model$variables))
  mapped_vars <- Map(f = function(var) {
    setNames(length(var$instances), var$type)
  }, model$variables)
  Reduce(f = function(acc, el) {
    acc[[names(el)]] <- acc[[names(el)]] + as.numeric(el)
    acc
  }, mapped_vars, init = list(continuous = 0, integer = 0,
                              binary = 0))
}

#' Variable types of a model
#'
#' One component for each variable in the correct order
#' @param model the model
#' @return a factor with levels binary, continuous, integer
#'
#' @examples
#' library(magrittr)
#' model <- MIPModel() %>%
#'   add_variable(x, type = "binary") %>%
#'   add_variable(y, type = "continuous") %>%
#'   add_variable(z, type = "integer")
#' variable_types(model)
#' @export
variable_types <- function(model) UseMethod("variable_types")

#' @export
variable_types.optimization_model <- function(model) {
  vars <- model$variables
  if (length(vars) == 0) {
    return(factor())
  }
  factor(unlist(lapply(sort(names(model$variables)), function(key) {
    var <- vars[[key]]
    rep.int(x = var$type, times = length(var$instances))
  })))
}

# a function to extract the lower/upper bounds of variables
extract_var_bounds_fun <- function(type) {
  stopifnot(type %in% c("lb", "ub"))
  default_val <- if (type == "lb") -Inf else Inf
  function(variables, keys) {
    unlist(lapply(keys, function(key) {
      var <- variables[[key]]
      bound <- if (length(var[[type]]) == 0) default_val else var[[type]]
      is_binary_var <- length(var[[type]]) == 0 && var$type == "binary"
      if (is_binary_var && type == "lb") bound <- 0
      if (is_binary_var && type == "ub") bound <- 1
      bound
    }))
  }
}

#' Variable lower and upper bounds of a model
#'
#' @param model the model
#'
#' @return a list with two components 'lower' and 'upper' each
#' having a numeric vector of bounds. One for each variable.
#'
#' @examples
#' library(magrittr)
#' model <- MIPModel() %>%
#'   add_variable(x, type = "binary") %>%
#'   add_variable(y, type = "continuous", lb = 2) %>%
#'   add_variable(z, type = "integer", ub = 3)
#' variable_bounds(model)
#' @export
variable_bounds <- function(model) UseMethod("variable_bounds")

#' @export
variable_bounds.optimization_model <- function(model) {
  model_vars <- model$variables
  if (length(model_vars) == 0) {
    return(list(lower = numeric(0), upper = numeric(0)))
  }
  extract_bounds_l <- extract_var_bounds_fun("lb")
  extract_bounds_u <- extract_var_bounds_fun("ub")
  keys <- sort(names(model$variables))
  list(
    lower = extract_bounds_l(model_vars, keys),
    upper = extract_bounds_u(model_vars, keys)
  )
}

#' Write the model as a .mps file
#'
#' @param model the model
#' @param modelFile the route and name of the .mps file. If none is give a random name is given.
#' @param returnMapping If the mapping should be returned or not for the user to use
#'  (Should be returned if it is going to be solved if it is going to be solved with CBC).
#'  By default it is 0 (not to return), change to 1 to return mapping.
#' @param modelName (optional). Sets the model name inside the file, default is "OMPR1"
#'
#'
#' @return It writes the model to the indicated file and returns the mapping used (as a data.frame)
#'
#'
#' @examples
#' library(magrittr)
#' result <- MIPModel() %>%
#'  add_variable(x, type = "integer") %>%
#'  add_variable(y, type = "continuous", lb = 0) %>%
#'  set_bounds(x, lb = 0) %>%
#'  set_objective(x + y, "max") %>%
#'  add_constraint(x + y <= 11.25)
#'
#' mapping <- write_MPS(model = model, file = "test.mps", modelname = "TEST")
#' @export
write_MPS <- function(model, modelFile=NULL, returnMapping=0, modelName="OMPR1") UseMethod("write_MPS")

#' @export
write_MPS.optimization_model <- function(model, modelFile, returnMapping=0, modelName="OMPR1"){
  # Control section
  if (class(model)!="optimization_model"){
    stop("The model object is not of class optimization_model defined by ompr")
  }

  # Prepare MPS data format
  mpsConsTypes <- c("E", "L", "G")
  consTypes <- c("==", "<=", ">=")

  mpsUpperBoundType <- c("UI", "BV", "UP")
  mpsLowBoundType <- c("LI", "", "LO")
  omprBoundType <- c("integer", "binary", "continuous")


  # Prepare model data section
  modelConstraints <- extract_constraints(model)
  modelVarNames <- variable_keys(model)
  modelObjFunction <- objective_function(model)
  modelVarType <- as.character(variable_types(model))
  modelVarBounds <- variable_bounds(model)

  matrixA <- modelConstraints$matrix
  vectorC <- modelObjFunction$vector

  if(model$objective$direction=="max"){
    vectorC <- -vectorC
  }

  numVar <- ncol(matrixA)
  namesVars <- stringr::str_c("V_",
                              formatC(seq(1:numVar),
                                      width=nchar(numVar),
                                      format="d",
                                      flag="0"))


  numCons <- nrow(matrixA)
  namesCons <- stringr::str_c("R_",
                              formatC(seq(1:numCons),
                                      width=nchar(numCons),
                                      format="d",
                                      flag="0"))



  colnames(matrixA) <- namesVars
  rownames(matrixA) <- namesCons

  dirCons <- modelConstraints$direction
  rhsCons <- modelConstraints$rhs

  dfCons <- data.frame(cons=namesCons,
                       dir=dirCons,
                       rhs=rhsCons,
                       stringsAsFactors=FALSE) %>%
    dplyr::mutate(mpsDir = mgsub(dir, consTypes, mpsConsTypes)) %>%
    dplyr::mutate(row = stringr::str_c(stringr::str_pad("", 1, pad=" ", side="left"),
                                       mpsDir,
                                       stringr::str_pad("",2,pad=" ", side="left"),
                                       cons)) %>%
    dplyr::mutate(rhsLine = stringr::str_c(stringr::str_pad(string = "", width =4, side = "left", pad = " "),
                                           "RHS1",
                                           stringr::str_pad(string = "", width = (10 - nchar("RHS1")), side = "left", pad = " "),
                                           cons,
                                           stringr::str_pad(string="", width = (10 - nchar(cons)), side = "left", pad = " " ),
                                           formatC(rhs, digits = 10, format="g")))

  dfVectorC <- data.frame(cons="OBJ",
                          vars=namesVars,
                          coef=vectorC,
                          stringsAsFactors = FALSE)

  dfMatrixA <- as.data.frame(as.table(matrixA), stringsAsFactors = FALSE) %>%
    dplyr::rename(cons=Var1, vars=Var2, coef=Freq) %>%
    rbind(dfVectorC) %>%
    dplyr::filter(coef!=0) %>%
    dplyr::arrange(vars, cons) %>%
    dplyr::mutate(line = stringr::str_c(stringr::str_pad(string = "", width =4, side = "left", pad = " "),
                                        vars,
                                        stringr::str_pad(string = "", width = (10 - nchar(vars)), side = "left", pad = " "),
                                        cons,
                                        stringr::str_pad(string="", width = (10 - nchar(cons)), side = "left", pad = " " ),
                                        formatC(coef, digits = 10, format="g")))

  dfVarBoundUp <-data.frame(type=modelVarType,
                            vars=namesVars,
                            bound="UPPER",
                            value=modelVarBounds$upper,
                            stringsAsFactors = FALSE) %>%
    dplyr::mutate(boundType = mgsub(type, omprBoundType, mpsUpperBoundType)) %>%
    dplyr::filter(!is.infinite(value))

  dfVarBoundLow <- data.frame(type=modelVarType,
                              vars=namesVars,
                              bound="LOWER",
                              value=modelVarBounds$lower,
                              stringsAsFactors = FALSE) %>%
    dplyr::mutate(boundType = mgsub(type, omprBoundType, mpsLowBoundType)) %>%
    dplyr::mutate(boundType = dplyr::if_else(value==-Inf, "MI", boundType)) %>%
    dplyr::mutate(value = dplyr::if_else(value==-Inf, "" , as.character(value))) %>%
    dplyr::filter(value != 0)

  dfVarBounds <- rbind(dfVarBoundUp, dfVarBoundLow) %>%
    dplyr::arrange(vars) %>%
    dplyr::mutate(line = str_c(stringr::str_pad("", width = 1, side = "left" ,pad = " "),
                               boundType,
                               stringr::str_pad("", width = 1, side = "left", pad = " "),
                               "BOUND1",
                               stringr::str_pad("", width = 4, side = "left", pad = " "),
                               vars,
                               stringr::str_pad(string="", width = (10 - nchar(vars)), side = "left", pad = " " ),
                               formatC(value, digits = 10, format="g")))


  # Write the first line of the file: NAME
  name <- stringr::str_c("NAME", stringr::str_pad("",11, "right"), modelName)
  write(name, file=modelFile)

  # Write the ROWS section
  write("ROWS", file=modelFile, append = TRUE)

  ## First we write the objective function
  obj <- stringr::str_c(stringr::str_pad("N", 2, "left"), stringr::str_pad("", 2, "right"), "OBJ")
  write(obj, file=modelFile, append = TRUE)

  ## Then we write the rest of the constraints
  cons <- dfCons$row
  write(cons, file=modelFile, append = TRUE)

  # Write the COLUMNS section
  write("COLUMNS", file=modelFile, append = TRUE)

  columns <- dfMatrixA$line
  write(columns, file=modelFile, append=TRUE)

  # Write the RHS section
  write("RHS", file=modelFile, append = TRUE)

  dfConsAux <- dfCons %>%
    filter(rhs != 0)

  rhsLines <- dfConsAux$rhsLine
  write(rhsLines, file=modelFile, append=TRUE)

  # Write the BOUNDS section
  write("BOUNDS", file=modelFile, append = TRUE)

  boundLines <- dfVarBounds$line
  write(boundLines, file=modelFile, append=TRUE)

  # Write ENDATA
  write("ENDATA", file=modelFile, append = TRUE)

  # Return mapping data.frame
  if (returnMapping==1){
    mapping <- data.frame(key = modelVarNames,
                                             variable = namesVars,
                                             stringsAsFactors = FALSE)
    mapping
    }


}

#' Calls cbc through a command to solve a MIP problem in a  .mps file
#'
#' @param mpsFile The .mps file containing the model
#' @param saveSolution If the solution should be stored in a specific file. 0 would save it in
#'        generic file that would be overwritten in the next solve call, 1 would save it in other file
#'        with name given or 'randomly' generated
#' @param solutionFile (optional) The name to give to the solution file.
#' @param timeLimit (optional) The time limit to give the solver, in seconds.
#' @param ... (optional) The rest of the parameters for the solver (to be implemented)
#'
#'
#'
#' @examples
#' library(magrittr)
#' result <- MIPModel() %>%
#'  add_variable(x, type = "integer") %>%
#'  add_variable(y, type = "continuous", lb = 0) %>%
#'  set_bounds(x, lb = 0) %>%
#'  set_objective(x + y, "max") %>%
#'  add_constraint(x + y <= 11.25)
#'
#' write_MPS(model = model, file = "test.mps", modelname = "TEST")
#'
#' solve_CBC(mpsFile="test.mps", saveSolution=1, solutionFile="testsolution.txt", timeLimit=60)
#' @export
solve_CBC <- function(mpsFile, saveSolution=0, solutionFile="", timeLimit=0, ...){

  if(!file.exists(mpsFile)){
    stop(paste("The file", mpsFile, "does not exist, please load an existing file"))
  }
  solveCommand <- "cbc"
  solveArgs <- mpsFile
  #command <- paste("cmd.exe /c cbc", mpsFile)

  if(timeLimit<0){

    stop("The time limit must be a positive integer")

  }
  else if(timeLimit!=0){

    solveArgs <- paste(solveArgs, "sec", as.character(timeLimit))

  }

  solveArgs <- paste(solveArgs, "solve")

  if(saveSolution == 0){
    solveArgs <- paste(solveArgs, "solu cbcsolution.txt")
  }
  else if(saveSolution == 1){
    if (solutionFile==""){
      randomSolutionFile <- paste0("cbcsolution_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),".txt")
      warning(paste("No solution file was given, the following file name was generated and assigned:", randomSolutionFile))
      solveArgs <- paste(solveArgs, "solu", randomSolutionFile)
    }
    else if(grepl(".txt", solutionFile)){
      solveArgs <- paste(solveArgs, "solu", solutionFile)
    }
    else {
      stop("The given name for the solution file is not right, please check it")
    }
  }

  system2(command=solveCommand, args=solveArgs)

}

#' Reads the solution file generated by CBC
#'
#' @param model The model which has been solved.
#' @param solutionFile The file containing the solution.
#' @param mapping The mapping generated by the write_MPS function. If none is given the random
#'        names generated during write_MPS are kept.
#'
#'
#' @examples
#' library(magrittr)
#' result <- MIPModel() %>%
#'  add_variable(x, type = "integer") %>%
#'  add_variable(y, type = "continuous", lb = 0) %>%
#'  set_bounds(x, lb = 0) %>%
#'  set_objective(x + y, "max") %>%
#'  add_constraint(x + y <= 11.25)
#'
#' write_MPS(model = model, file = "test.mps", modelname = "TEST")
#'
#' solve_CBC(mpsFile="test.mps", saveSolution=1, solutionFile="testsolution.txt", timeLimit=60)
#'
#' read_CBCsolution("testsolution.txt")
#' @export
read_CBCsolution <- function(model, solutionFile, mapping=NULL){

  status <- readLines(solutionFile, n=1)

  if (grepl(x=status, pattern="Unbounded")){
    solutionType <- "unbounded"
  }
  else if (grepl(x=status, pattern="Optimal")){
    solutionType <- "optimal"
  }
  else if (grepl(x=status, pattern="Infeasible")){
    solutionType <- "infeasible"
  }
  else if (grepl(x=status, pattern="time")){
    solutionType <- "integer solution"
  }

  solution_internal <- read.table(solutionFile,
                                  col.names = c("row", "variable", "value", "objvalue"),
                                  skip=1,
                                  stringsAsFactors = FALSE)

  if (is.data.frame(mapping) && length(intersect(solution_internal$variable,
                                                 mapping$variable))>0)
    {
    solution_internal <- solution_internal %>%
      left_join(mapping, by="variable") %>%
      mutate(variable=key)
  }
  else if(is.null(mapping)){

  }
  else{
    stop("The mapping is not a data.frame or is not the correct mapping for this solution file")
  }

  objValue <- solution_internal %>% group_by() %>% summarise(obj = sum(value*objvalue)) %>% as.numeric()

  solution_vector <- as.vector(solution_internal$value)
  names(solution_vector) <- solution_internal$variable


  new_solution(model, objValue, solutionType, solution=solution_vector)


}


#' @title Sends a model to solve to CBC and gets the solution back
#' @description
#' It uses the write_MPS, solve_CBC and read_CBCSolution functions in sequence with some default values
#'
#'
#' @param model The ompr model to solve
#' @param modelFile The file where the model should be stored (Model.mps by default)
#' @param saveSolution Boolean to save the solution in a specific file
#' @param solutionFile The file containing the solution (cbcsolution.txt by default)
#' @param modelName If the model should have an specific name inside the mps file (ompr1 by default)
#' @param timeLimit Time limit for the solver
#' @param ... More cbc argumets (to be implemented)
#'
#'
#' @examples
#' library(magrittr)
#' result <- MIPModel() %>%
#'  add_variable(x, type = "integer") %>%
#'  add_variable(y, type = "continuous", lb = 0) %>%
#'  set_bounds(x, lb = 0) %>%
#'  set_objective(x + y, "max") %>%
#'  add_constraint(x + y <= 11.25)
#'
#' solution <- CBCsolution(model=result, timeLimit=60)
#'
#' @export
CBCsolution <- function(model,
                        modelFile="Model.mps",
                        saveSolution=1,
                        solutionFile="cbcsolution.txt",
                        modelName="ompr1",
                        timeLimit,
                        ...)
  {

  mapping <- write_MPS(model=model, modelFile=modelFile, returnMapping=1, modelName=modelName)

  solve_CBC(mpsFile=modelFile, saveSolution=saveSolution, solutionFile=solutionFile, timeLimit = timeLimit)

  solution <- read_CBCsolution(solutionFile=solutionFile, mapping = mapping)

  solution

}
