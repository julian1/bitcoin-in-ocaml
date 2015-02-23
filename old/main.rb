
# needs to be done transactionally,
# permissions. a k is owned by someone. ??? k cannot be modified.

class State



	def initialize()
    @@state = { }
    # we want to add the code for an initial contract here,
  end

  def put( key, value)
    #
    @@state[key] = value

  end

  def get( key)
    @@state[key]
  end

end

s = State.new()

s.put( '/home/genesis/k', 123 )  # shouuld be a proc...

puts s.get( '/home/genesis/k' )

# so a tx, will just be inputs to a k?.

# actually blockchain state is slightly different
# state -> tx -> state

# the tx is a function or is it just arguments ... that may refer to other functions in the block chain...
# on execution.

# the whole thing is functional- there's just a need to be able to seralize function code (not needed if in mem).

# we need to be able to pass functions to functions... easily. eg. a tx that refers to a contract.
# and transactions
# and testing of permissions?

# for example we would bind the user credentials into the monad, as a permissions thing... 

# how do we control the ability to increase someone elses assets but not decrease.
# need an operator to give. not a + code. 


tx = {
  'k' => '/home/genesis/k',
  'value' => 10
};




