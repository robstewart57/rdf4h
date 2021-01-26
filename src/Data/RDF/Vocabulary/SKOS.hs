{-# LANGUAGE OverloadedStrings #-}

-- | Vocabulary for elements of the [Simple Simple Knowledge Organization System (SKOS)](http://www.w3.org/2004/02/skos/)
module Data.RDF.Vocabulary.SKOS where

import Data.RDF.Namespace (skos)
import Data.RDF.Types (Node)
import Data.RDF.Vocabulary.Utils (mkUnode)

concept :: Node
concept = mkUnode skos "Concept"

conceptScheme :: Node
conceptScheme = mkUnode skos "ConceptScheme"

collection :: Node
collection = mkUnode skos "Collection"

orderedCollection :: Node
orderedCollection = mkUnode skos "OrderedCollection"

prefLabel :: Node
prefLabel = mkUnode skos "prefLabel"

altLabel :: Node
altLabel = mkUnode skos "altLabel"

broader :: Node
broader = mkUnode skos "broader"

narrower :: Node
narrower = mkUnode skos "narrower"

hasTopConcept :: Node
hasTopConcept = mkUnode skos "hasTopConcept"

member :: Node
member = mkUnode skos "member"

hiddenLabel :: Node
hiddenLabel = mkUnode skos "hiddenLabel"

inScheme :: Node
inScheme = mkUnode skos "inScheme"

topConceptOf :: Node
topConceptOf = mkUnode skos "topConceptOf"

memberList :: Node
memberList = mkUnode skos "memberList"

notation :: Node
notation = mkUnode skos "notation"

changeNote :: Node
changeNote = mkUnode skos "changeNote"

definition :: Node
definition = mkUnode skos "definition"

editorialNote :: Node
editorialNote = mkUnode skos "editorialNote"

example :: Node
example = mkUnode skos "example"

historyNote :: Node
historyNote = mkUnode skos "historyNote"

note :: Node
note = mkUnode skos "note"

scopeNote :: Node
scopeNote = mkUnode skos "scopeNote"

broaderTransitive :: Node
broaderTransitive = mkUnode skos "broaderTransitive"

narrowerTransitive :: Node
narrowerTransitive = mkUnode skos "narrowerTransitive"

related :: Node
related = mkUnode skos "related"

semanticRelation :: Node
semanticRelation = mkUnode skos "semanticRelation"

broadMatch :: Node
broadMatch = mkUnode skos "broadMatch"

closeMatch :: Node
closeMatch = mkUnode skos "closeMatch"

exactMatch :: Node
exactMatch = mkUnode skos "exactMatch"

mappingRelation :: Node
mappingRelation = mkUnode skos "mappingRelation"

narrowMatch :: Node
narrowMatch = mkUnode skos "narrowMatch"

relatedMatch :: Node
relatedMatch = mkUnode skos "relatedMatch"
