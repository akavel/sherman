
local scheme = require 'scheme'

local script='trivial.s'

scheme.add_primitive('require-library', function(library, collection)
	scheme.load(("collects/%s/%s"):format(collection, library))
end, 3)

scheme.run('(parameterize ((current-namespace sherman-namespace)) (load "'..script..'"))')
scheme.run('(current-namespace sherman-namespace)')

