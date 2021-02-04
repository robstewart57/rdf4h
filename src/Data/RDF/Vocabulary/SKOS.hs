{-# LANGUAGE OverloadedStrings #-}

-- | Vocabulary for elements of the [Simple Simple Knowledge Organization System (SKOS)](http://www.w3.org/2004/02/skos/)
module Data.RDF.Vocabulary.SKOS where

import Data.RDF.Namespace (mkUri, skos)
import Data.RDF.Types (Node, unode)

concept :: Node
concept = unode $ mkUri skos "Concept"

conceptScheme :: Node
conceptScheme = unode $ mkUri skos "ConceptScheme"

collection :: Node
collection = unode $ mkUri skos "Collection"

orderedCollection :: Node
orderedCollection = unode $ mkUri skos "OrderedCollection"

prefLabel :: Node
prefLabel = unode $ mkUri skos "prefLabel"

altLabel :: Node
altLabel = unode $ mkUri skos "altLabel"

broader :: Node
broader = unode $ mkUri skos "broader"

narrower :: Node
narrower = unode $ mkUri skos "narrower"

hasTopConcept :: Node
hasTopConcept = unode $ mkUri skos "hasTopConcept"

member :: Node
member = unode $ mkUri skos "member"

hiddenLabel :: Node
hiddenLabel = unode $ mkUri skos "hiddenLabel"

inScheme :: Node
inScheme = unode $ mkUri skos "inScheme"

topConceptOf :: Node
topConceptOf = unode $ mkUri skos "topConceptOf"

memberList :: Node
memberList = unode $ mkUri skos "memberList"

notation :: Node
notation = unode $ mkUri skos "notation"

changeNote :: Node
changeNote = unode $ mkUri skos "changeNote"

definition :: Node
definition = unode $ mkUri skos "definition"

editorialNote :: Node
editorialNote = unode $ mkUri skos "editorialNote"

example :: Node
example = unode $ mkUri skos "example"

historyNote :: Node
historyNote = unode $ mkUri skos "historyNote"

note :: Node
note = unode $ mkUri skos "note"

scopeNote :: Node
scopeNote = unode $ mkUri skos "scopeNote"

broaderTransitive :: Node
broaderTransitive = unode $ mkUri skos "broaderTransitive"

narrowerTransitive :: Node
narrowerTransitive = unode $ mkUri skos "narrowerTransitive"

related :: Node
related = unode $ mkUri skos "related"

semanticRelation :: Node
semanticRelation = unode $ mkUri skos "semanticRelation"

broadMatch :: Node
broadMatch = unode $ mkUri skos "broadMatch"

closeMatch :: Node
closeMatch = unode $ mkUri skos "closeMatch"

exactMatch :: Node
exactMatch = unode $ mkUri skos "exactMatch"

mappingRelation :: Node
mappingRelation = unode $ mkUri skos "mappingRelation"

narrowMatch :: Node
narrowMatch = unode $ mkUri skos "narrowMatch"

relatedMatch :: Node
relatedMatch = unode $ mkUri skos "relatedMatch"
