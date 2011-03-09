alter table forum add column "description" varchar(512);

update forum
       set description="Discussions autour des aspects techniques de la photographie comme la gestion de la lumière ou la profondeur de champ mais aussi les questions sur les logiciels de développement."
       where id=2;

update forum
       set description="Discussions autour du matériel photographique, nouveautés, achat, vente..."
       where id=3;

update forum
       set description="Discussions à propos du site lui même, les évolutions possibles, les rapports de bug."
       where id=4;
