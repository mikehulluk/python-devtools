#!/usr/bin/python
# -*- coding: utf-8 -*-

from sqlalchemy import create_engine, ForeignKey
from sqlalchemy import Column, Date, Integer, String
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import relationship


from sqlalchemy.orm import sessionmaker

from pdt.filelocations import PDTFileLocations



engine = create_engine(PDTFileLocations.get_patch_sqlalchemy_url(),
                       echo=False)
Base = declarative_base()


class Patch(Base):

    """"""

    __tablename__ = 'patches'

    id = Column(Integer, primary_key=True)
    target_filename = Column(String)
    patch_data = Column(String)
    patch_set = Column(Integer, ForeignKey('patchsets.id'))

    def __init__(self, target_filename, patch_data):
        self.target_filename = target_filename
        self.patch_data = patch_data


class PatchSet(Base):

    __tablename__ = 'patchsets'
    id = Column(Integer, primary_key=True)
    name = Column(String)
    release_date = Column(Date)
    patches = relationship('Patch', cascade='all,delete,delete-orphan')

    def __str__(self):
        return 'Patchset: %s (%d patches)' % (self.name,
                len(self.patches))


# Setup the classes with SQL-alchemy
Base.metadata.create_all(engine)
Session = sessionmaker(bind=engine)
session = Session()

